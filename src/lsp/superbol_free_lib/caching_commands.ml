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
    cache: CACHE.t option;
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

let locate Common_args.{ project_root; platform; _ } given_filename =
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
  { filename; project; contents; cache = Option.map snd (cache_for rootdir) }

let analyze_located Common_args.{ platform; analysis_overrides = overrides; _ }
    { filename; project; contents; cache } =
  let analysis, hit =
    match cache with
    | Some cache ->
        ANALYSIS.cached_analysis cache ~platform ~overrides ~filename ~contents
          project
    | None ->
        ANALYSIS.analyze ~platform ~overrides ~filename ~contents
          project, false
  in
  { filename; project; contents; analysis; hit }

let analyze_located_for_lsp
    Common_args.{ platform; analysis_overrides = overrides; _ }
    { filename; project; contents; cache; _ } =
  let analysis, hit =
    match cache with
    | Some cache ->
        ANALYSIS.cached_analysis_for_lsp cache ~platform ~overrides ~filename
          ~contents project
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

(** [derived_entry common ~label ~inputs located] is the cache, and key of the
    derived entry, for [located], along with the keys of the analysis entries
    (normal, and for the LSP) it may derive from. *)
let derived_entry Common_args.{ analysis_overrides = overrides; _ }
    ~label ~inputs ({ filename; project; contents; cache }: located_file) =
  match cache,
        ANALYSIS.inputs ~overrides ~filename ~contents project,
        ANALYSIS.key ~overrides ~filename ~contents project,
        ANALYSIS.key_for_lsp ~overrides ~filename ~contents project with
  | Some cache, Some analysis_inputs, Some analysis_key, Some lsp_key ->
      let key = CACHE.shallow_key ~label ~inputs:(analysis_inputs @ inputs) in
      Some (cache, key, analysis_key, lsp_key)
  | _ ->
      None

(** [analyze_located_n_witness common ~cache ~analysis_key ~lsp_key located] is
    the analysis of [located], along with the key of the cache entry it is held
    in: the analysis for the LSP if [cache] holds one, or else the (normal)
    analysis, computed and stored on a miss. *)
let analyze_located_n_witness
    (Common_args.{ platform; _ } as common)
    ~cache ~analysis_key ~lsp_key
    ({ filename; project; contents; _ } as located: located_file)
  : ANALYSIS.t analyzed_file * _ =
  let resolve = RECORDER.resolver ~platform () in
  match ANALYSIS.lookup_for_lsp cache ~key:lsp_key ~resolve with
  | Some a ->
      { filename; project; contents; hit = true;
        analysis = ANALYSIS.of_lsp_artifacts a }, lsp_key
  | None ->
      analyze_located common located, analysis_key

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
let derive_from_file
    (Common_args.{ platform; _ } as common)
    ~label ~inputs ~(derive: ANALYSIS.t analyzed_file -> 'a) filename
  : 'a * bool =
  let located = locate common filename in
  match derived_entry common ~label ~inputs located with
  | Some (cache, key, analysis_key, lsp_key) ->
      let resolve = RECORDER.resolver ~platform () in
      begin match CACHE.lookup cache ~key ~resolve
                    ~read_item:CACHING.read_item with
      | Ok Some v ->
          v, true
      | Ok None | Error _ ->
          let analyzed, analysis_key =
            analyze_located_n_witness common ~cache ~analysis_key ~lsp_key
              located in
          let v = derive analyzed in
          ignore @@
          CACHE.store cache ~key
            ~label:(Printf.sprintf "%s:%s" label @@
                    Superbol_project.relative_path_for ~filename
                      located.project)
            ~witnesses:[WITNESS.sub analysis_key]
            ~write_item:CACHING.write_item v;
          v, false
      end
  | None ->
      derive @@ analyze_located common located, false

(** [lookup_derived common ~label ~inputs filename] retrieves a value
    previously recorded by {!derive_from_file} with the same [label] and
    [inputs], without analyzing [filename]; [None] on a miss. *)
let lookup_derived (Common_args.{ platform; _ } as common)
    ~label ~inputs filename : 'a option =
  match derived_entry common ~label ~inputs @@ locate common filename with
  | Some (cache, key, _, _) ->
      let resolve = RECORDER.resolver ~platform () in
      begin match CACHE.lookup cache ~key ~resolve
                    ~read_item:CACHING.read_item with
      | Ok v -> v
      | Error _ -> None
      end
  | None ->
      None

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
