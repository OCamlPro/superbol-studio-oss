(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ezcmd.V2
open EZCMD.TYPES

module DIAGS = Cobol_common.Diagnostics

open Cobol_preproc.Options
open Cobol_parser.Options

type t = {
  platform: Cobol_common.Platform.TYPES.platform;
  project_root: string option;
  preproc_options: preproc_options;
  parser_options: parser_options;
  pretty_progress: 'a. 'a Pretty.proc;
  analysis_overrides: Superbol_project.Analysis.overrides;
}

type progress_display =
  | Hide
  | Show
  | Log
  | Log_stderr

let showable =
  [
    ["pending"], `Pending;
  ]

let iter_comma_separated_spec ~showable ~option_name ~f spec =
  EzString.split_simplify spec ',' |>
  List.fold_left begin fun unknowns spec ->
    let spec' = String.(lowercase_ascii @@ trim @@ spec) in
    match
      List.find_map begin fun (sl, tag) ->
        if List.mem spec' sl then Some tag else None
      end showable
    with
    | Some tag -> f tag; unknowns
    | None -> StringSet.add spec unknowns
  end StringSet.empty |>
  fun unknowns ->
  if not (StringSet.is_empty unknowns) then
    raise @@ Stdlib.Arg.Bad
      Pretty.(to_string "@[Unknown@ arguments@ for@ `%s':@ %a@]"
                option_name
                (list ~fopen:"" ~fsep:",@ " ~fclose:"" string)
                (StringSet.elements unknowns))

(** Departures from project configurations that are explicitly requested on the
    command line, for commands that analyze documents within their projects. *)
let analysis_overrides ~config ~conf ~dialect ~format ~libpath ~libexts ~env
    ~recovery ~show : Superbol_project.Analysis.overrides =
  let absolute dir =
    if Ez_file.V1.EzFile.is_absolute dir then dir
    else Ez_file.V1.EzFile.concat (Sys.getcwd ()) dir
  in
  (* Describes the configuration for cache keys.  The configuration file
     itself is identified by its contents whenever it can be found as given;
     files it includes are not accounted for, as with dialects. *)
  let override_cobol_config =
    match conf, dialect with
    | "", None ->
        None
    | "", Some d ->
        Some (config, "dialect:" ^ Cobol_config.DIALECT.to_string d)
    | file, _ when Sys.file_exists file ->
        Some (config, Pretty.to_string "conf:%s:%s" (absolute file)
                (Digest.to_hex @@ Digest.file file))
    | file, _ ->
        Some (config, "conf:" ^ file)
  in
  {
    override_cobol_config;
    override_source_format = format;
    override_libpath = List.map absolute libpath;
    override_libexts = libexts;
    override_env = env;
    override_recovery = recovery;
    override_show = show;
  }

(* At toplevel in the (very unlikely) case where several calls to [get] are
   performed by a single CLI instance. *)
let last_progress_msg_len = ref 0

let get () =
  let project_root = ref None in
  let conf = ref "" in
  let dialect = ref None in
  let format = ref Cobol_config.Auto in
  let explicit_format = ref false in
  let formats = ["free"; "Free"; "FREE";
                 "fixed"; "Fixed"; "FIXED";
                 "cobol85"; "COBOL85";
                 "variable"; "VARIABLE";
                 "xopen"; "XOPEN"; "XOpen";
                 "xcard"; "xCard"; "XCARD";
                 "crt"; "CRT";
                 "terminal"; "Terminal"; "TERMINAL";
                 "cobolx"; "Cobolx"; "CobolX"; "COBOLX";
                 "auto"; "AUTO"] in
  let libpath = ref ["."] in
  let extra_libpath = ref [] in
  let search_path = ref [] in
  let libexts = ref Cobol_common.Copybook.copybook_extensions in
  let extra_libexts = ref [] in
  let definitions = ref [] in
  let recovery = ref true in
  let explicit_recovery = ref false in
  let show = ref [`Pending] in                                     (* default *)
  let explicit_show = ref false in
  let silence spec =
    explicit_show := true;
    iter_comma_separated_spec ~showable ~option_name:"--silence" spec
      ~f:(fun tag -> show := List.filter ((<>) tag) !show)
  in
  let progress_display_opt = ref None  in

  let args = [
    ["project-root"; "root"], Arg.File (fun f -> project_root := Some f),
    EZCMD.info ~docv:"DIR" "Set the root of the project (autodetected on a \
                            per-file basis otherwise)";

    ["conf"], Arg.Set_string conf,
    EZCMD.info ~docv:"CONF_FILE" "Set the configuration file to be used";

    ["dialect"; "std"], Arg.Symbol
      (Cobol_config.DIALECT.all_canonical_names,
       fun d -> dialect := Some (Cobol_config.DIALECT.of_string d)),
    EZCMD.info ~docv:"DIALECT"
      "Set the dialect to bu used (overriden by `--conf` if used)";

    ["source-format"],
    Arg.Symbol (formats, fun f ->
        explicit_format := true;
        format := try Cobol_config.Options.format_of_string f with
          | Invalid_argument _ ->
              DIAGS.Now.warn Fmt.stderr
                "Unkown source format: %s, setting to default" f;
              Auto),
    EZCMD.info ~docv:"SOURCE_FORMAT"
      "Set the format of source code; allowed values are: { FIXED (the default), \
       FREE}\nOverrides `format` from configuration file if present.";

    ["D"],
    Arg.String (fun s -> definitions := s :: !definitions),
    EZCMD.info ~docv:"VAR=VAL"
      "Define a pre-processor variable VAR, with value VAL";

    ["free"], Arg.Unit (fun () -> explicit_format := true;
                         format := SF SFFree),
    EZCMD.info "Shorthand for `--source-format FREE`";

    ["recovery"], Arg.Symbol (["true"; "false"], fun b ->
        explicit_recovery := true;
        recovery := bool_of_string b),
    EZCMD.info ~docv:"BOOL" @@
    Pretty.to_string "Enable/disable parser recovery after syntax errors \
                      (default: %b)" !recovery;

    ["silence"], Arg.String silence,
    EZCMD.info "Silence specific messages";

    ["config-dir"], Arg.String (fun s ->
        search_path := !search_path @ [s]),
    EZCMD.info ~docv:"DIR" "Add DIR to config search path";

    ["I"], Arg.String (fun s -> libpath := !libpath @ [s];
                        extra_libpath := !extra_libpath @ [s]),
    EZCMD.info ~docv:"DIR" "Add DIR to library search path";

    ["ext"], Arg.String (fun s -> libexts := !libexts @ [s];
                          extra_libexts := !extra_libexts @ [s]),
    EZCMD.info ~docv:"EXT" @@
    Pretty.to_string
      "Add EXT as a filename extension for copybook resolution (default: %a)"
      Fmt.(list ~sep:sp @@ fmt "`%s`")
      Cobol_common.Copybook.copybook_extensions;

    ["progress"], Arg.Symbol (["hide"; "force"; "log"; "log-err"], function
      | "hide" -> progress_display_opt := Some Hide
      | "force" -> progress_display_opt := Some Show
      | "log" -> progress_display_opt := Some Log
      | "log-err" -> progress_display_opt := Some Log_stderr
      | _ -> assert false),
    EZCMD.info ~docv:"MODE"
      "Set how to display progress of the command.\n\
      MODE can be hide, force, log or log-err.
      By default progress is displayed when stderr is a TTY.";

  ] in

  let get () =
    let verbose = !Globals.verbosity > 0 in
    let search_path = !search_path in
    let config =
      DIAGS.show_n_forget ~platform:Superbol_platform.record @@
      match !conf, !dialect with
      | "", None ->
          DIAGS.result Cobol_config.default
      | "", Some d ->
          Cobol_config.from_dialect ~verbose ~search_path d
      | s, None ->
          Cobol_config.from_file ~verbose ~search_path s
      | _ ->
          Pretty.failwith "Flags `--conf` and `--dialect` or `--std` cannot be \
                           used together"
    in
    let source_format =
      match !format with
      | Auto ->
          let module Config = (val config: Cobol_config.T) in
          Config.format#value
      | SF _ -> !format
    in
    let recovery =
      if !recovery
      then EnableRecovery { silence_benign_recoveries = false }
      else DisableRecovery
    in
    let env =
      List.fold_right begin fun definition env ->
        try
          let eqsign = String.index definition '=' in
          let var = String.sub definition 0 eqsign
          and def = String.(sub definition (eqsign + 1)
                              (length definition - eqsign - 1)) in
          let value = Cobol_preproc.Env.numeric_or_else_alphanum_of_string def in
          (* TODO: Check numerics: i.e, no quotes & proper format. *)
          fst @@                             (* drop definition used for logs *)
          Cobol_preproc.Env.define_process_parameter
            (Cobol_preproc.Env.var var)
            { src_payload = value;
              src = Process_parameter } env
        with Not_found ->
          Pretty.failwith "Invalid argument `%s' given to flag `-D`" definition
      end !definitions Cobol_preproc.Env.empty
    in
    let platform =
      { Superbol_platform.record with verbosity = !Globals.verbosity }
    in
    let progress_display = match !progress_display_opt with
      | None -> if Unix.isatty Unix.stderr then Show else Hide
      | Some p -> p
    in
    let analysis_overrides =
      analysis_overrides ~config ~conf:!conf ~dialect:!dialect
        ~format:(if !explicit_format then Some !format else None)
        ~libpath:!extra_libpath ~libexts:!extra_libexts ~env
        ~recovery:(if !explicit_recovery then Some recovery else None)
        ~show:(if !explicit_show then Some !show else None)
    in
    (* Pretty.error "@[Preprocessor environment:@;<1 2>@[%a@]@]@." *)
    (*   Cobol_preproc.Env.pp env; *)
    { platform;
      project_root = !project_root;
      preproc_options = { platform; config; source_format;
                          exec_preprocs = EXEC_MAP.empty;
                          copybook_lookup_config =
                            Cobol_common.Copybook.lookup_config !libpath
                              ~libexts:!libexts;
                          env };
      analysis_overrides;
      parser_options = { config; recovery; verbose; show = !show;
                         exec_scanners = Superbol_preprocs.exec_scanners };
      pretty_progress = match progress_display with
        | Hide -> Pretty.sink
        | Show -> fun fmt ->
            Format.kasprintf (fun s ->
                let len = String.length s in
                let padlen = max 0 (!last_progress_msg_len - len) in
                last_progress_msg_len := len;
                Printf.eprintf "%s%*s\r%!" s padlen "")
              ("@[<h>" ^^ fmt ^^ "@]")
        | Log -> fun fmt -> Pretty.out ("@[" ^^ fmt ^^ "@]@.")
        | Log_stderr -> fun fmt -> Pretty.error ("@[" ^^ fmt ^^ "@]@.") }
  in
  get, args
