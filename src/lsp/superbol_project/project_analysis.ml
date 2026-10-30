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

open EzCompat                                                    (* StringMap *)
open Project.TYPES
open Project_config.TYPES

module DIAGS = Cobol_common.Diagnostics
module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module RECORDER = Superbol_cache.Recorder
module WITNESS = Superbol_cache.Witness

type t =
  {
    copybook: bool;
    checked: Cobol_typeck.Outputs.t option;
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

let copybook =
  {
    copybook = true;
    checked = None;
    parsing_diags = Cobol_parser.Diagnostics.ALL.none;
    typeck_diags = Cobol_typeck.Diagnostics.none;
  }

let diagnostics { parsing_diags; typeck_diags; _ } =
  DIAGS.Set.union
    (Cobol_parser.Diagnostics.ALL.translate parsing_diags)
    (Cobol_typeck.Diagnostics.translate typeck_diags)

(* --- Results for the LSP --- *)

type for_lsp =
  {
    lsp_copybook: bool;
    lsp_pplog: Cobol_preproc.Trace.log;
    lsp_tokens: Cobol_parser.Outputs.tokens_with_locs;
    lsp_comments: Cobol_preproc.Text.comments StringMap.t;
    lsp_ignored: Cobol_common.Srcloc.lexloc list;
    lsp_checked: Cobol_typeck.Outputs.t option;
    lsp_parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    lsp_typeck_diags: Cobol_typeck.Diagnostics.t;
  }

let of_artifacts_for_lsp ~checked ~parsing_diags ~typeck_diags
    ~parsing_artifacts:Cobol_parser.Outputs.{ pplog; tokens;
                                              rev_comments; rev_ignored } =
  {
    lsp_copybook = false;
    lsp_pplog = pplog;
    lsp_tokens = Lazy.force tokens;
    lsp_comments = rev_comments;
    lsp_ignored = rev_ignored;
    lsp_checked = checked;
    lsp_parsing_diags = parsing_diags;
    lsp_typeck_diags = typeck_diags;
  }

let artifacts { lsp_pplog; lsp_tokens; lsp_comments; lsp_ignored; _ } =
  Cobol_parser.Outputs.{ pplog = lsp_pplog;
                         tokens = lazy lsp_tokens;
                         rev_comments = lsp_comments;
                         rev_ignored = lsp_ignored }

let copybook_for_lsp =
  {
    lsp_copybook = true;
    lsp_pplog = Cobol_preproc.Trace.empty;
    lsp_tokens = [];
    lsp_comments = StringMap.empty;
    lsp_ignored = [];
    lsp_checked = None;
    lsp_parsing_diags = Cobol_parser.Diagnostics.ALL.none;
    lsp_typeck_diags = Cobol_typeck.Diagnostics.none;
  }

let of_lsp_artifacts { lsp_copybook; lsp_checked;
                 lsp_parsing_diags; lsp_typeck_diags; _ } =
  {
    copybook = lsp_copybook;
    checked = lsp_checked;
    parsing_diags = lsp_parsing_diags;
    typeck_diags = lsp_typeck_diags;
  }

(* --- Options --- *)

type overrides =
  {
    override_cobol_config: (Cobol_config.t * string) option;
    override_source_format: Cobol_config.source_format_spec option;
    override_libpath: string list;
    override_libexts: string list;
    override_env: Cobol_preproc.Env.t;
    override_recovery: Cobol_parser.Options.recovery option;
    override_show: [`Pending] list option;
  }

let no_overrides =
  {
    override_cobol_config = None;
    override_source_format = None;
    override_libpath = [];
    override_libexts = [];
    override_env = Cobol_preproc.Env.empty;
    override_recovery = None;
    override_show = None;
  }

(* Overrides that bear on the project configuration are applied to a copy of
   it, so that everything that derives from it (copybook detection and lookup,
   cache keys) sees them. *)
let with_overrides ?(overrides = no_overrides) ({ config; _ } as project) =
  let { override_cobol_config; override_source_format;
        override_libpath; override_libexts; _ } = overrides in
  if Option.is_none override_cobol_config &&
     Option.is_none override_source_format &&
     override_libpath = [] && override_libexts = []
  then project
  else
    let config =
      { config with
        cobol_config =
          (match override_cobol_config with
           | Some (cobol_config, _) -> cobol_config
           | None -> config.cobol_config);
        source_format =
          Option.value override_source_format ~default:config.source_format;
        source_format_by_extension =
          (if override_source_format = None
           then config.source_format_by_extension
           else []);
        libpath =
          config.libpath @
          List.map (fun dir -> RelativeToProjectRoot dir) override_libpath;
        libexts = config.libexts @ override_libexts }
    in
    { project with config }

let detect_copybook ~platform ?overrides ~filename ~contents project =
  Project.detect_copybook ~platform ~filename ~contents @@
  with_overrides ?overrides project

let preproc_options ~platform ?overrides ~filename project =
  let { config; _ } as project = with_overrides ?overrides project in
  Cobol_preproc.Options.{
    (default ~platform) with
    copybook_lookup_config =
      Project.copybook_lookup_config_for ~filename project;
    config = config.cobol_config;
    source_format = Project_config.source_format_for ~filename config;
    env = (Option.value overrides ~default:no_overrides).override_env;
  }

let parser_options ?(overrides = no_overrides) project =
  let { config; _ } = with_overrides ~overrides project in
  let options =
    Cobol_parser.Options.{
      (default ~exec_scanners:Superbol_preprocs.exec_scanners) with
      recovery = EnableRecovery { silence_benign_recoveries = true };
      config = config.cobol_config;
    }
  in
  { options with
    recovery =
      Option.value overrides.override_recovery ~default:options.recovery;
    show = Option.value overrides.override_show ~default:options.show }

(* Describes the choices made in the functions above that are not part of the
   project configuration: [exec_scanners] is a function, so the options cannot
   be digested as they are.  To be changed along with any of these choices. *)
let options_signature =
  "exec-scanners=superbol_preprocs;recovery=silence-benign"

let parse parse_fun ~platform ?overrides ~filename ~contents project =
  parse_fun
    ~options:(parser_options ?overrides project) @@
  Cobol_preproc.preprocessor
    ~options:(preproc_options ~platform ?overrides ~filename project) @@
  Cobol_preproc.Input.string ~filename contents

let typecheck ?overrides ~parsing_diags project parsed =
  let { config; _ } = with_overrides ?overrides project in
  let module Config = (val config.cobol_config) in
  let Cobol_typeck.Results.{ result = checked; diags = typeck_diags } =
    Cobol_typeck.compilation_group parsed
      ~fold_exec_block':Superbol_preprocs.Esql.fold_exec_block'
      ~options:{ binary_size = Config.binary_size#value }
  in
  let typeck_diags =
    (* Do not report typeck diagnostics in case of syntax or pre-processing
       errors: *)
    if Cobol_parser.Diagnostics.ALL.has_errors parsing_diags
    then Cobol_typeck.Diagnostics.none
    else typeck_diags
  in
  checked, typeck_diags

let analyze ~platform ?overrides ~filename ~contents project =
  if detect_copybook ~platform ?overrides ~filename ~contents project
  then copybook
  else
    let Cobol_parser.Outputs.{ result = parsed; diags = parsing_diags } =
      parse Cobol_parser.parse_simple
        ~platform ?overrides ~filename ~contents project
    in
    let checked, typeck_diags =
      typecheck ?overrides ~parsing_diags project parsed
    in
    { copybook = false; checked = Some checked; parsing_diags; typeck_diags }

(* Same as [analyze], with a parse that collects artifacts. *)
let analyze_for_lsp ~platform ?overrides ~filename ~contents project =
  if detect_copybook ~platform ?overrides ~filename ~contents project
  then copybook_for_lsp
  else
    let Cobol_parser.Outputs.{ result = parsed; diags = parsing_diags } =
      parse Cobol_parser.parse_with_artifacts
        ~platform ?overrides ~filename ~contents project
    in
    let checked, typeck_diags =
      typecheck ?overrides ~parsing_diags project parsed
    in
    of_artifacts_for_lsp ~checked:(Some checked) ~parsing_diags ~typeck_diags
      ~parsing_artifacts:(Cobol_parser.artifacts parsed)

(* --- Caching --- *)

(* Everything about the project and options that bears on how a document is
   preprocessed and parsed.  It takes part in every key rather than being
   witnessed, because settings pushed by an LSP client never reach a file we
   could witness.  An overriding COBOL configuration is represented by its
   description, as it cannot be digested itself. *)
type config_inputs = Superbol_cache.Types.digest

let config_inputs ?(overrides = no_overrides) project =
  let { config; _ } = with_overrides ~overrides project in
  try
    Option.some @@
    Digest.string @@
    Marshal.to_string
      (Cobol_config.DIALECT.to_string @@
       Cobol_config.dialect config.cobol_config,
       config.source_format,
       config.source_format_by_extension,
       config.libpath,
       config.libexts,
       Option.map snd overrides.override_cobol_config,
       overrides.override_env,
       overrides.override_recovery,
       overrides.override_show,
       options_signature) []
  with Invalid_argument _ ->
    None         (* nothing here should be functional, but if ever, just fail *)

type keys =
  {
    inputs: Superbol_cache.Types.digest list;
    key: Superbol_cache.Types.key;
    key_for_lsp: Superbol_cache.Types.key;
  }

(* The document's contents take part in the keys, rather than being witnessed:
   entries are published without ever replacing one another, so a document that
   is edited has to land under a key of its own.

   Both kinds of results are cached on their own, from the same inputs: neither
   is derived from the other, which would make a hit on one require the other
   to be held. *)
let document_keys config ~filename ~contents =
  let inputs =
    [Digest.string @@ WITNESS.canonical_path filename;
     Digest.string contents;
     config]
  in
  {
    inputs;
    key = CACHE.shallow_key ~label:"analysis" ~inputs;
    key_for_lsp = CACHE.shallow_key ~label:"analysis-for-lsp" ~inputs;
  }

let keys ?overrides ~filename ~contents project =
  Option.map (document_keys ~filename ~contents) @@
  config_inputs ?overrides project

let inputs { inputs; _ } = inputs
let key { key; _ } = key
let key_for_lsp { key_for_lsp; _ } = key_for_lsp

let store_any cache ~key ~label ~witnesses v =
  CACHE.store cache ~key ~label ~witnesses ~write_item:CACHING.write_item v

let lookup_any cache ~key ~resolve =
  match CACHE.lookup cache ~key ~resolve ~read_item:CACHING.read_item with
  | Ok result -> result
  | Error _ -> None

let store_for_lsp cache ~key ~label ~witnesses (a: for_lsp) =
  store_any cache ~key ~label ~witnesses a

let lookup_for_lsp cache ~key ~resolve : for_lsp option =
  lookup_any cache ~key ~resolve

(* Looks [key] up, or else performs [analyze] while recording what [platform]
   is asked, and stores the result under [key]. *)
let lookup_or_analyze cache ~platform ~resolve ~key ~label ~analyze =
  match lookup_any cache ~key ~resolve with
  | Some v ->
      v, true
  | None ->
      let recorder = RECORDER.create ~platform in
      let v = analyze ~platform:(RECORDER.platform recorder) in
      ignore @@
      store_any cache ~key ~label ~witnesses:(RECORDER.witnesses recorder) v;
      v, false

let cached_analysis_n_key cache ~platform ?overrides
    ~keys:{ key; key_for_lsp; _ } ~filename ~contents project =
  let resolve = RECORDER.resolver ~platform () in
  match lookup_for_lsp cache ~key:key_for_lsp ~resolve with
  | Some a ->
      of_lsp_artifacts a, true, key_for_lsp
  | None ->
      let v, hit =
        lookup_or_analyze cache ~platform ~resolve ~key
          ~label:(Project.relative_path_for ~filename project)
          ~analyze:(analyze ?overrides ~filename ~contents project)
      in
      v, hit, key

let cached_analysis cache ~platform ?overrides ~keys ~filename ~contents
    project =
  let v, hit, _ =
    cached_analysis_n_key cache ~platform ?overrides ~keys ~filename ~contents
      project
  in
  v, hit

let cached_analysis_for_lsp cache ~platform ?overrides
    ~keys:{ key_for_lsp; _ } ~filename ~contents project =
  lookup_or_analyze cache ~platform ~key:key_for_lsp
    ~resolve:(RECORDER.resolver ~platform ())
    ~label:(Project.relative_path_for ~filename project)
    ~analyze:(analyze_for_lsp ?overrides ~filename ~contents project)
