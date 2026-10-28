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

type analyzed_file =
  {
    filename: string;                                             (* absolute *)
    project: Superbol_project.t;
    contents: string;
    analysis: ANALYSIS.t;
    hit: bool;                              (* whether found in project cache *)
  }

let locate Common_args.{ platform; _ } filename =
  let filename =
    if EzFile.is_absolute filename then filename
    else platform.getcwd () // filename        (* filenames take part in keys *)
  in
  let project = Project.for_filename filename in
  let rootdir = Superbol_project.(string_of_rootdir @@ rootdir project) in
  let contents = EzFile.read_text_file filename in
  filename, project, contents, Option.map snd (cache_for rootdir)

let analyze_located Common_args.{ platform; analysis_overrides = overrides; _ }
    ~filename ~project ~contents cache =
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

(** [analyze_file common filename] analyzes [filename] according to the
    configuration of its project and the overrides in [common], reusing or else
    filling the project's cache. *)
let analyze_file common filename : analyzed_file =
  let filename, project, contents, cache = locate common filename in
  analyze_located common ~filename ~project ~contents cache

(** [derive_from_file common ~label ~inputs ~derive filename] is [derive]
    applied to the result of {!analyze_file} on [filename].  The result is
    cached in the project's cache, along with the analysis it is derived from:
    a hit on the derived value requires the analysis itself to be a hit, and
    does not entail retrieving the latter.

    [label] identifies the derivation, and [inputs] should hold digests of
    everything [derive] depends upon besides the analysis result.  The Boolean
    indicates a hit.

    Note that [derive] must return a value that holds no closure, and be fully
    determined by the producer (as identified by the cache), the analysis, and
    [inputs]. *)
let derive_from_file
    (Common_args.{ platform; analysis_overrides = overrides; _ } as common)
    ~label ~inputs ~(derive: analyzed_file -> 'a) filename : 'a * bool =
  let filename, project, contents, cache = locate common filename in
  let compute () =
    derive @@ analyze_located common ~filename ~project ~contents cache in
  match cache,
        ANALYSIS.inputs ~overrides ~filename ~contents project,
        ANALYSIS.key ~overrides ~filename ~contents project with
  | Some cache, Some analysis_inputs, Some analysis_key ->
      let key = CACHE.shallow_key ~label ~inputs:(analysis_inputs @ inputs) in
      let resolve = RECORDER.resolver ~platform () in
      begin match CACHE.lookup cache ~key ~resolve
                    ~read_item:CACHING.read_item with
      | Ok Some v ->
          v, true
      | Ok None | Error _ ->
          let v = compute () in
          let label =
            Printf.sprintf "%s:%s" label @@
            Superbol_project.relative_path_for ~filename project in
          ignore @@
          CACHE.store cache ~key ~label ~witnesses:[WITNESS.sub analysis_key]
            ~write_item:CACHING.write_item v;
          v, false
      end
  | _ ->
      compute (), false

(** [report common ~ppf analysis] reports analysis diagnostics on [ppf]. *)
let report Common_args.{ platform; pretty_progress; _ } ~ppf
    { filename; analysis; hit; _ } =
  pretty_progress "%s@ `%s'%s" (if hit then "Found in cache:" else "Analyzed:")
    filename (if analysis.copybook then " (copybook)" else "");
  DIAGS.sink_result ~platform ~ppf @@
  DIAGS.result ~diags:(ANALYSIS.diagnostics analysis) ()

(** [analyze_n_report common ~ppf files] analyzes [files] with {!analyze_file},
    in turn, and reports diagnostics on [ppf]. *)
let analyze_n_report (Common_args.{ pretty_progress; _ } as common) ~ppf files =
  List.iter begin fun filename ->
    pretty_progress "Analyzing@ `%s'" filename;
    report common ~ppf @@ analyze_file common filename
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
