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
    pplog: Cobol_preproc.Trace.log;
    tokens: Cobol_parser.Outputs.tokens_with_locs;
    comments: Cobol_preproc.Text.comments StringMap.t;
    ignored: Cobol_common.Srcloc.lexloc list;
    checked: Cobol_typeck.Outputs.t option;
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

let of_artifacts ~checked ~parsing_diags ~typeck_diags
    ~parsing_artifacts:Cobol_parser.Outputs.{ pplog; tokens;
                                              rev_comments; rev_ignored } =
  {
    copybook = false;
    pplog;
    tokens = Lazy.force tokens;
    comments = rev_comments;
    ignored = rev_ignored;
    checked;
    parsing_diags;
    typeck_diags;
  }

let artifacts { pplog; tokens; comments; ignored; _ } =
  Cobol_parser.Outputs.{ pplog;
                         tokens = lazy tokens;
                         rev_comments = comments;
                         rev_ignored = ignored }

let copybook =
  {
    copybook = true;
    pplog = Cobol_preproc.Trace.empty;
    tokens = [];
    comments = StringMap.empty;
    ignored = [];
    checked = None;
    parsing_diags = Cobol_parser.Diagnostics.ALL.none;
    typeck_diags = Cobol_typeck.Diagnostics.none;
  }

let diagnostics { parsing_diags; typeck_diags; _ } =
  DIAGS.Set.union
    (Cobol_parser.Diagnostics.ALL.translate parsing_diags)
    (Cobol_typeck.Diagnostics.translate typeck_diags)

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

let parse ~platform ?overrides ~filename ~contents project =
  Cobol_parser.parse_with_artifacts
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
      parse ~platform ?overrides ~filename ~contents project
    in
    let checked, typeck_diags =
      typecheck ?overrides ~parsing_diags project parsed
    in
    of_artifacts ~checked:(Some checked) ~parsing_diags ~typeck_diags
      ~parsing_artifacts:(Cobol_parser.artifacts parsed)

(* --- Caching --- *)

(* Everything about the project and options that bears on how a document is
   preprocessed and parsed.  It takes part in every key rather than being
   witnessed, because settings pushed by an LSP client never reach a file we
   could witness.  An overriding COBOL configuration is represented by its
   description, as it cannot be digested itself. *)
let config_digest ?(overrides = no_overrides) project =
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

(* The document's contents take part in the key, rather than being witnessed:
   entries are published without ever replacing one another, so a document that
   is edited has to land under a key of its own. *)
let inputs ?overrides ~filename ~contents project =
  Option.map begin fun config_digest ->
    [Digest.string @@ WITNESS.canonical_path filename;
     Digest.string contents;
     config_digest]
  end (config_digest ?overrides project)

let key ?overrides ~filename ~contents project =
  Option.map (fun inputs -> CACHE.shallow_key ~label:"analysis" ~inputs) @@
  inputs ?overrides ~filename ~contents project

let store cache ~key ~label ~witnesses (t: t) =
  CACHE.store cache ~key ~label ~witnesses ~write_item:CACHING.write_item t

let lookup cache ~key ~resolve : t option =
  match CACHE.lookup cache ~key ~resolve ~read_item:CACHING.read_item with
  | Ok result -> result
  | Error _ -> None

let cached_analysis cache ~platform ?overrides ~filename ~contents project =
  match key ?overrides ~filename ~contents project with
  | None ->
      analyze ~platform ?overrides ~filename ~contents project, false
  | Some key ->
      let resolve = RECORDER.resolver ~platform () in
      match lookup cache ~key ~resolve with
      | Some t ->
          t, true
      | None ->
          let recorder = RECORDER.create ~platform in
          let t =
            analyze ~platform:(RECORDER.platform recorder)
              ?overrides ~filename ~contents project
          in
          let label = Project.relative_path_for ~filename project in
          ignore @@
          store cache ~key ~label ~witnesses:(RECORDER.witnesses recorder) t;
          t, false
