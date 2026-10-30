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

let cache_for rootdir =
  Superbol_project.Cache.for_ ~rootdir
    ~storage_location:Superbol_project.Cache.default_storage_location

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

let locate
    Common_args.{ project_root; platform; analysis_overrides = overrides; _ }
    given_filename =
  let filename =                               (* filenames take part in keys *)
    if EzFile.is_absolute given_filename
    then given_filename
    else platform.getcwd () // given_filename
  in
  let project = match project_root with
    | Some dirname -> Project.in_ ~dirname ()
    | None -> Project.for_filename filename
  in
  let rootdir = Superbol_project.(string_of_rootdir @@ rootdir project) in
  let contents = EzFile.read_text_file filename in
  if given_filename != filename then
    Cobol_common.Srcloc.NAMING.register_given_filename ~given_filename
      ~filename;
  let cached =
    Option.bind (cache_for rootdir) @@ fun (_, cache) ->
    Option.map (fun keys -> cache, keys) @@
    ANALYSIS.keys ~overrides ~filename ~contents project
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
  let analysis, hit =
    match cached with
    | Some (cache, keys) ->
        ANALYSIS.cached_analysis_for_lsp cache ~platform ~overrides ~keys
          ~filename ~contents project
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
let analyze_file_for_lsp common filename : ANALYSIS.for_lsp analyzed_file =
  analyze_located_for_lsp common @@ locate common filename

let of_lsp_artifacts_file
    ({ analysis; _ } as file: ANALYSIS.for_lsp analyzed_file) =
  { file with analysis = ANALYSIS.of_lsp_artifacts analysis }

(** [derived_entry ~label ~inputs located] is the cache, and key of the derived
    entry, for [located], along with the keys of the analysis it may derive
    from. *)
let derived_entry ~label ~inputs ({ cached; _ }: located_file) =
  Option.map begin fun (cache, keys) ->
    let inputs = ANALYSIS.inputs keys @ inputs in
    cache, CACHE.shallow_key ~label ~inputs, keys
  end cached

(** [analyze_located_n_witness common ~cache ~keys located] is the analysis
    of [located], along with the key of the cache entry it is held in: the
    analysis for the LSP if [cache] holds one, or else the (normal) analysis,
    computed and stored on a miss. *)
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
  | Some (cache, key, keys) ->
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
          ignore @@
          CACHE.store cache ~key
            ~label:(Printf.sprintf "%s:%s" label @@
                    Superbol_project.relative_path_for ~filename:located.filename
                      located.project)
            ~witnesses:[WITNESS.sub analysis_key]
            ~write_item:CACHING.write_item v;
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
    {!derive_from_file}, except that it also returns the analysis of
    [filename], which is retrieved from the project's cache in case of a hit
    on the derived value.  Use this to only cache, as derived value, what can
    not be obtained from the analysis directly. *)
let derive_n_analyze_file common ~label ~inputs ~derive filename
  : 'a * bool * ANALYSIS.t analyzed_file =
  let v, hit, analyzed = derive_located common ~label ~inputs ~derive @@
    locate common filename in
  v, hit, Lazy.force analyzed

let lookup_located (Common_args.{ platform; _ } as common) ~label ~inputs
    located : ('a * ANALYSIS.t analyzed_file Lazy.t) option =
  match derived_entry ~label ~inputs located with
  | Some (cache, key, keys) ->
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

(** [lookup_derived common ~label ~inputs filename] retrieves a value
    previously recorded by {!derive_from_file} with the same [label] and
    [inputs], without analyzing [filename]; [None] on a miss. *)
let lookup_derived common ~label ~inputs filename : 'a option =
  Option.map fst @@ lookup_located common ~label ~inputs @@
  locate common filename

(** [lookup_derived_n_analysis common ~label ~inputs filename] is like
    {!lookup_derived}, except that it also returns the analysis of [filename]
    in case of a hit, which is retrieved from the project's cache (it is
    computed and stored anew if the cache no longer holds it). *)
let lookup_derived_n_analysis common ~label ~inputs filename
  : ('a * ANALYSIS.t analyzed_file) option =
  Option.map (fun (v, analyzed) -> v, Lazy.force analyzed) @@
  lookup_located common ~label ~inputs @@ locate common filename

(** [report common ~ppf analysis] reports analysis diagnostics on [ppf]. *)
let report Common_args.{ platform; pretty_progress; _ } ~ppf
    ({ filename; analysis; hit; _ }: ANALYSIS.t analyzed_file) =
  pretty_progress "%s:@ `%s'%s" (if hit then "Found in cache" else "Analyzed")
    filename (if analysis.copybook then " (copybook)" else "");
  DIAGS.sink_result ~platform ~ppf @@
  DIAGS.result ~diags:(ANALYSIS.diagnostics analysis) ()

(** [analyze_n_report common ~for_lsp ~ppf files] analyzes [files] in turn,
    and reports diagnostics on [ppf].  Files are analyzed with
    {!analyze_file_for_lsp} if [for_lsp] holds (so the cache is filled for the
    LSP), or else with {!analyze_file}. *)
let analyze_n_report (Common_args.{ pretty_progress; _ } as common) ~for_lsp
    ~ppf files =
  List.iter begin fun filename ->
    pretty_progress "Analyzing@ `%s'" filename;
    report common ~ppf @@
    if for_lsp
    then of_lsp_artifacts_file @@ analyze_file_for_lsp common filename
    else analyze_file common filename
  end files

(** [with_analysis_results common ~f ~ppf files] analyzes [files] with
    {!analyze_file}, in turn, reports analysis diagnostics on [ppf], and then
    passes the analysis result to [f]. *)
let with_analysis_results common ~f ~ppf files =
  List.iter begin fun filename ->
    let res = analyze_file common filename in
    report common ~ppf res;
    f res
  end files

(** [with_typeck_outputs common ~f ~ppf files] acts like
    [with_analysis_results], except [f] is given the type-checking result as
    second argument. Therefore [f] is only called if the type-checking
    succeeds. *)
let with_typeck_outputs ~f =
  with_analysis_results ~f:begin fun res ->
    Option.iter (f res) res.analysis.checked
  end
