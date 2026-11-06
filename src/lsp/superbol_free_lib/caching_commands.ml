(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open EzFile.OP

module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module RECORDER = Superbol_cache.Recorder
module WITNESS = Superbol_cache.Witness
module DIAGS = Cobol_common.Diagnostics
module ANALYSIS = Superbol_project.Analysis
module CACHED_DIAGS = Superbol_project.Cached_diagnostics
module SEMTOKS = Superbol_project.Semantic_tokens

(** How projects are set up, and where their caches live. *)
type setup =
  {
    layout: Superbol_project.layout;
    storage_location: Superbol_project.TYPES.cache_storage_location;
    client_settings: (string * Yojson.Safe.t) list option;
    (** Settings of an LSP client, for projects without configuration file *)
    encoding: Cobol_common.Text_encoding.t option;
    (** Encoding the LSP client decodes files with; overrides the
        [source_encoding] of projects. *)
  }

let default_setup =
  {
    layout = Superbol_project.Layout.default;
    storage_location = Superbol_project.Cache.default_storage_location;
    client_settings = None;
    encoding = None;
  }

(** [lsp_like_setup ?storage_directory ?client_settings ?encoding ()] sets
    projects up as an LSP server started with [--storage-directory
    storage_directory] does, whose client sends [client_settings] and decodes
    files with [encoding]. *)
let lsp_like_setup ?storage_directory ?client_settings ?encoding () =
  let layout =
    Superbol_project.Layout.create ?shared_storage_directory:storage_directory
      ()
  in
  let storage_location =
    Result.fold ~ok:Fun.id ~error:(Pretty.failwith "%s") @@
    Superbol_project.Cache.storage_location ~project_layout:layout ()
      ?fallback_storage_directory:storage_directory
  in
  { layout; storage_location; client_settings; encoding }

let cache_for ?(setup = default_setup) rootdir =
  Superbol_project.Cache.for_ ~rootdir ~storage_location:setup.storage_location

(* As the LSP server does (see `use_client_config_for' in
   `cobol_lsp/lsp_server.ml'), client settings only apply to projects that have
   no configuration file.  These are configured once per root directory, apart
   from the projects {!Superbol_project.for_} shares. *)
let configured_projects = Hashtbl.create 1

let settings_reporter =
  Cobol_lsp.Project.{
    report_dialect_diags = begin fun ~dialect_name:_ ~dialect_file:_ _ diags ->
      DIAGS.Set.pp ~platform:Superbol_platform.record Fmt.stderr diags
    end;
    report_config_error = begin fun e ->
      Pretty.error "%a@." Superbol_project.Diagnostics.pp_error e
    end;
    report_invalid_setting = begin fun ~key msg ->
      Pretty.error "Invalid@ value@ for@ setting@ %s:@ %s@." key msg
    end;
  }

let with_client_settings ~setup ~platform project =
  match setup.client_settings with
  | None ->
      project
  | Some _ when Sys.file_exists project.Superbol_project.config_filename ->
      project
  | Some assoc ->
      let rootdir = Superbol_project.rootdir project in
      let key = Superbol_project.string_of_rootdir rootdir in
      match Hashtbl.find_opt configured_projects key with
      | Some project ->
          project
      | None ->
          let project =
            Superbol_project.with_default_config ~rootdir ~layout:setup.layout
          in
          ignore @@ Cobol_lsp.Project.update_project_config assoc project
            ~platform ~reporter:settings_reporter;
          Hashtbl.add configured_projects key project;
          project

type located_file =
  {
    filename: string;
    project: Superbol_project.t;
    contents: string;
    cached: (CACHE.t * ANALYSIS.keys) option;    (* project cache, and keys *)
  }

(** ['a] is either {!ANALYSIS.t} or {!ANALYSIS.for_lsp}. *)
type 'a analyzed_file =
  {
    filename: string;                                             (* absolute *)
    project: Superbol_project.t;
    contents: string;
    analysis: 'a;
    hit: bool;                              (* whether found in project cache *)
  }

(* The project's cache, along with the inputs that keys of analyses of its
   documents with some overrides share.  Projects are loaded once per root
   directory ({!Superbol_project.for_}), and so is their cache: these are
   computed again only for another project (e.g. if its configuration was
   reloaded), or other overrides. *)
let cached_configs = Hashtbl.create 1

let cached_config ~setup ~overrides project =
  let rootdir = Superbol_project.(string_of_rootdir @@ rootdir project) in
  match Hashtbl.find_opt cached_configs rootdir with
  | Some (project', overrides', cached_config)
    when project' == project && overrides' == overrides ->
      cached_config
  | _ ->
      let cached_config =
        Option.bind (cache_for ~setup rootdir) @@ fun (_, cache) ->
        Option.map (fun config -> cache, config) @@
        ANALYSIS.config_inputs ~overrides project
      in
      Hashtbl.replace cached_configs rootdir (project, overrides, cached_config);
      cached_config

let locate ?(setup = default_setup)
    Common_args.{ project_root; platform; analysis_overrides = overrides; _ }
    given_filename =
  let filename =                               (* filenames take part in keys *)
    if EzFile.is_absolute given_filename
    then given_filename
    else platform.getcwd () // given_filename
  in
  let layout = setup.layout in
  let project = with_client_settings ~setup ~platform @@ match project_root with
    | Some dirname -> Project.in_ ~dirname ~layout ()
    | None -> Project.for_filename filename ~layout
  in
  (* Documents are keyed on their contents: decode them as the LSP client does,
     for keys to match those of the documents it sends to the server. *)
  let contents =
    Superbol_project.decode_source ?encoding:setup.encoding project @@
    EzFile.read_text_file filename
  in
  if given_filename != filename then
    Cobol_common.Srcloc.NAMING.register_given_filename ~given_filename
      ~filename;
  let cached =
    Option.map begin fun (cache, config) ->
      cache, ANALYSIS.document_keys config ~filename ~contents
    end (cached_config ~setup ~overrides project)
  in
  { filename; project; contents; cached }

let analyze_located Common_args.{ platform; analysis_overrides = overrides; _ }
    { filename; project; contents; cached } =
  let analysis, hit =
    match cached with
    | Some (cache, keys) ->
        ANALYSIS.cached_analysis cache ~platform ~overrides ~keys ~filename
          ~contents project
    | None ->
        ANALYSIS.analyze ~platform ~overrides ~filename ~contents
          project, false
  in
  { filename; project; contents; analysis; hit }

let analyze_located_for_lsp
    Common_args.{ platform; analysis_overrides = overrides; _ }
    { filename; project; contents; cached } =
  (* Semantic tokens are computed (and cached) as the LSP does, as it is the LSP
     that will reuse them. *)
  let semantic_tokens = Cobol_lsp.Semtoks.whole in
  let analysis, hit =
    match cached with
    | Some (cache, keys) ->
        let analysis, hit as res =
          SEMTOKS.cached_analysis_for_lsp cache ~platform ~overrides ~keys
            ~semantic_tokens ~filename ~contents project
        in
        (* Workspace symbols and diagnostics are likewise stored as the LSP
           does, so that it can retrieve them without opening the document. *)
        let label = Superbol_project.relative_path_for ~filename project in
        Cobol_lsp.Workspace_symbols.store_for_analysis cache ~keys ~filename
          ~label ~fresh:(not hit) analysis;
        CACHED_DIAGS.store_for_analysis cache ~keys ~label
          ~fresh:(not hit) analysis;
        res
    | None ->
        ANALYSIS.analyze_for_lsp ~platform ~overrides ~filename ~contents
          project, false
  in
  { filename; project; contents; analysis; hit }

(** [analyze_file common filename] analyzes [filename] according to the
    configuration of its project and the overrides in [common], reusing or else
    filling the project's cache. *)
let analyze_file common filename : ANALYSIS.t analyzed_file =
  analyze_located common @@ locate common filename

(** [analyze_file_for_lsp common filename] is like {!analyze_file}, except that
    it provides an analysis for the LSP.  Use this to get all artifacts the LSP
    would need, or to fill the cache for the LSP. *)
let analyze_file_for_lsp ?setup common filename
  : ANALYSIS.for_lsp analyzed_file =
  analyze_located_for_lsp common @@ locate ?setup common filename

let of_lsp_artifacts_file
    ({ analysis; _ } as file: ANALYSIS.for_lsp analyzed_file) =
  { file with analysis = ANALYSIS.of_lsp_artifacts analysis }

(** [derived_entry ~label ~inputs located] is the cache, key and slot of the
    derived entry, for [located], along with the keys of the analysis it may
    derive from. *)
let derived_entry ~label ~inputs ({ cached; _ }: located_file) =
  Option.map begin fun (cache, keys) ->
    cache,
    CACHE.shallow_key ~label ~inputs:(ANALYSIS.inputs keys @ inputs),
    ANALYSIS.slot ~label ~inputs keys,
    keys
  end cached

(** [analyze_located_n_witness common ~cache ~keys located] is the analysis of
    [located], along with the key of the cache entry it is held in: the analysis
    for the LSP if [cache] holds one, or else the (normal) analysis, computed
    and stored on a miss. *)
let analyze_located_n_witness
    Common_args.{ platform; analysis_overrides = overrides; _ }
    ~cache ~keys ({ filename; project; contents; _ }: located_file)
  : ANALYSIS.t analyzed_file * _ =
  let analysis, hit, key =
    ANALYSIS.cached_analysis_n_key cache ~platform ~overrides ~keys ~filename
      ~contents project
  in
  { filename; project; contents; analysis; hit }, key

let derive_located
    (Common_args.{ platform; _ } as common)
    ~label ~inputs ~(derive: ANALYSIS.t analyzed_file -> 'a) located
  : 'a * bool * ANALYSIS.t analyzed_file Lazy.t =
  match derived_entry ~label ~inputs located with
  | Some (cache, key, slot, keys) ->
      let resolve = RECORDER.resolver ~platform () in
      begin match CACHE.lookup cache ~key ~resolve
                    ~read_item:CACHING.read_item with
      | Ok Some v ->
          v, true, lazy begin
            fst @@
            analyze_located_n_witness common ~cache ~keys
              located
          end
      | Ok None | Error _ ->
          let analyzed, analysis_key =
            analyze_located_n_witness common ~cache ~keys
              located in
          let v = derive analyzed in
          begin
            match
              CACHE.store cache ~key
                ~label:(Printf.sprintf "%s:%s" label @@
                        Superbol_project.relative_path_for
                          ~filename:located.filename located.project)
                ~witnesses:[WITNESS.sub analysis_key]
                ~write_item:CACHING.write_item v
            with
            | Ok () -> ignore @@ CACHE.supersede cache ~slot [key]
            | Error _ -> ()
          end;
          v, false, Lazy.from_val analyzed
      end
  | None ->
      let analyzed = analyze_located common located in
      derive analyzed, false, Lazy.from_val analyzed

(** [derive_from_file common ~label ~inputs ~derive filename] is [derive]
    applied to the result of {!analyze_file} on [filename].  The result is
    cached in the project's cache, along with the analysis it is derived from
    (which may be one for the LSP if the cache holds it): a hit on the derived
    value requires the analysis itself to be a hit, and does not entail
    retrieving the latter.

    [label] identifies the derivation, and [inputs] should hold digests of
    everything [derive] depends upon besides the analysis result.  The Boolean
    indicates a hit.

    Note that [derive] must return a value that holds no closure, and be fully
    determined by the producer (as identified by the cache), the analysis, and
    [inputs]. *)
let derive_from_file common ~label ~inputs ~derive filename : 'a * bool =
  let v, hit, _ = derive_located common ~label ~inputs ~derive @@
    locate common filename in
  v, hit

(** [derive_n_analyze_file common ~label ~inputs ~derive filename] is like
    {!derive_from_file}, except that it also returns the analysis of [filename],
    which is retrieved from the project's cache in case of a hit on the derived
    value.  Use this to only cache, as derived value, what can not be obtained
    from the analysis directly. *)
let derive_n_analyze_file common ~label ~inputs ~derive filename
  : 'a * bool * ANALYSIS.t analyzed_file =
  let v, hit, analyzed = derive_located common ~label ~inputs ~derive @@
    locate common filename in
  v, hit, Lazy.force analyzed

let lookup_located (Common_args.{ platform; _ } as common) ~label ~inputs
    located : ('a * ANALYSIS.t analyzed_file Lazy.t) option =
  match derived_entry ~label ~inputs located with
  | Some (cache, key, _, keys) ->
      let resolve = RECORDER.resolver ~platform () in
      begin match CACHE.lookup cache ~key ~resolve
                    ~read_item:CACHING.read_item with
      | Ok Some v ->
          Some (v, lazy begin
              fst @@
              analyze_located_n_witness common ~cache ~keys
                located
            end)
      | Ok None | Error _ ->
          None
      end
  | None ->
      None

(** [lookup_derived common ~label ~inputs filename] retrieves a value previously
    recorded by {!derive_from_file} with the same [label] and [inputs], without
    analyzing [filename]; [None] on a miss. *)
let lookup_derived common ~label ~inputs filename : 'a option =
  Option.map fst @@ lookup_located common ~label ~inputs @@
  locate common filename

(** [lookup_derived_n_analysis common ~label ~inputs filename] is like
    {!lookup_derived}, except that it also returns the analysis of [filename] in
    case of a hit, which is retrieved from the project's cache (it is computed
    and stored anew if the cache no longer holds it). *)
let lookup_derived_n_analysis common ~label ~inputs filename
  : ('a * ANALYSIS.t analyzed_file) option =
  Option.map (fun (v, analyzed) -> v, Lazy.force analyzed) @@
  lookup_located common ~label ~inputs @@ locate common filename

(** [report common ~error_status ~show_diagnostics ~ppf analysis] reports
    analysis diagnostics on [ppf], unless [show_diagnostics] is [false].
    Errors among them, shown or not, make the program exit with a non-zero
    status, unless [error_status] is [false].  Both default to [true]. *)
let report Common_args.{ platform; pretty_progress; _ }
    ?(error_status = true) ?(show_diagnostics = true)
    ~ppf ({ filename; analysis; hit; _ }: ANALYSIS.t analyzed_file) =
  pretty_progress "%s:@ `%s'%s" (if hit then "Found in cache" else "Analyzed")
    (Cobol_common.Srcloc.NAMING.given_filename filename)
    (if analysis.copybook then " (copybook)" else "");
  let ppf =
    if show_diagnostics then ppf
    else Format.make_formatter (fun _ _ _ -> ()) ignore
  in
  DIAGS.sink_result ~platform ~ppf ~set_status:error_status @@
  DIAGS.result ~diags:(ANALYSIS.diagnostics analysis) ()

(** [analyze_n_report ?setup ?error_status ?show_diagnostics common ~for_lsp
    ~ppf files] analyzes [files] in turn, and reports diagnostics on [ppf].
    Files are analyzed with {!analyze_file_for_lsp} if [for_lsp] holds (so the
    cache is filled for the LSP), or else with {!analyze_file}; [setup] only
    applies to the former.  See {!report} for [error_status] and
    [show_diagnostics]; files that cannot be analyzed are reported on [ppf]
    regardless. *)
let analyze_n_report ?setup ?error_status ?show_diagnostics
    (Common_args.{ pretty_progress; _ } as common) ~for_lsp ~ppf files =
  List.iter begin fun filename ->
    pretty_progress "Analyzing@ `%s'"
      (Cobol_common.Srcloc.NAMING.given_filename filename);
    match
      if for_lsp
      then of_lsp_artifacts_file @@ analyze_file_for_lsp ?setup common filename
      else analyze_file common filename
    with
    | analyzed ->
        report common ?error_status ?show_diagnostics ~ppf analyzed
    | exception (Out_of_memory | Stack_overflow | Sys.Break as e) ->
        raise e
    | exception e ->
        (* Carry on with the remaining files, but exit with a status that tells
           some file could not be analyzed, regardless of [error_status]. *)
        Cobol_common.record_failure ();
        Pretty.print ppf "@[<hov>Could@ not@ analyze@ `%s':@ %a@]@."
          (Cobol_common.Srcloc.NAMING.given_filename filename)
          Pretty.failure e
  end files

(** [fold_analysis_results common ~f ~ppf files acc] analyzes [files] with
    {!analyze_file}, in turn, reports analysis diagnostics on [ppf], and then
    passes the analysis result and an accumulator to [f]. *)
let fold_analysis_results common ~f ~ppf files acc =
  List.fold_left begin fun acc filename ->
    let res = analyze_file common filename in
    report common ~ppf res;
    f res acc
  end acc files

(** [fold_typeck_outputs common ~f ~ppf files acc] acts like
    [fold_analysis_results], except [f] is given the type-checking result as
    second argument. Therefore [f] is only called if the type-checking
    succeeds. *)
let fold_typeck_outputs ~f files acc =
  fold_analysis_results ~f:begin fun res ->
    Option.fold ~none:Fun.id ~some:(f res) res.analysis.checked
  end files acc
