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
  preproc_options: preproc_options;
  parser_options: parser_options;
  pretty_verbose: 'a. 'a Pretty.proc;
}

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

let get ?(verbose_on = `Stdout) () =
  let conf = ref "" in
  let dialect = ref None in
  let format = ref Gnucobol_config.Auto in
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
  let libexts = ref Cobol_common.Copybook.copybook_extensions in
  let definitions = ref [] in
  let recovery = ref true in
  let show = ref [`Pending] in                                     (* default *)
  let silence spec =
    iter_comma_separated_spec ~showable ~option_name:"--silence" spec
      ~f:(fun tag -> show := List.filter ((<>) tag) !show)
  in

  let args = [

    ["conf"], Arg.Set_string conf,
    EZCMD.info ~docv:"CONF_FILE" "Set the configuration file to be used";

    ["dialect"; "std"], Arg.Symbol
      (Gnucobol_config.DIALECT.all_canonical_names,
       fun d -> dialect := Some (Gnucobol_config.DIALECT.of_string d)),
    EZCMD.info ~docv:"DIALECT"
      "Set the dialect to bu used (overriden by `--conf` if used)";

    ["source-format"],
    Arg.Symbol (formats, fun f ->
        format := try Gnucobol_config.Options.format_of_string f with
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

    ["free"], Arg.Unit (fun () -> format := SF SFFree),
    EZCMD.info "Shorthand for `--source-format FREE`";

    ["recovery"], Arg.Set_bool recovery,
    EZCMD.info ~docv:"BOOL" @@
    Pretty.to_string "Enable/disable parser recovery after syntax errors \
                      (default: %b)" !recovery;

    ["silence"], Arg.String silence,
    EZCMD.info "Silence specific messages";

    ["I"], Arg.String (fun s -> libpath := !libpath @ [s]),
    EZCMD.info ~docv:"DIR" "Add DIR to library search path";

    ["ext"], Arg.String (fun s -> libexts := !libexts @ [s]),
    EZCMD.info ~docv:"EXT" @@
    Pretty.to_string
      "Add EXT as a filename extension for copybook resolution (default: %a)"
      Fmt.(list ~sep:sp @@ fmt "`%s`")
      Cobol_common.Copybook.copybook_extensions;
  ] in

  let get () =
    let verbose = !Globals.verbosity > 0 in
    let config =
      DIAGS.show_n_forget @@
      match !conf, !dialect with
      | "", None ->
          DIAGS.result Gnucobol_config.default
      | "", Some d ->
          Gnucobol_config.from_dialect ~verbose d
      | s, None ->
          Gnucobol_config.from_file ~verbose s
      | _ ->
          Pretty.failwith "Flags `--conf` and `--dialect` or `--std` cannot be \
                           used together"
    in
    let source_format =
      match !format with
      | Auto ->
          let module Config = (val config: Gnucobol_config.T) in
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
          (* TODO: Check numerics: i.e, no quotes & proper format. *)
          Cobol_preproc.Env.define_process_parameter
            (Cobol_preproc.Env.var var)
            (Alphanum { pp_payload = def;
                        pp_loc = Process_parameter }) env
        with Not_found ->
          Pretty.failwith "Invalid argument `%s' given to flag `-D`" definition
      end !definitions Cobol_preproc.Env.empty
    in
    (* Pretty.error "@[Preprocessor environment:@;<1 2>@[%a@]@]@." *)
    (*   Cobol_preproc.Env.pp env; *)
    let config = Gnucobol_config.COMMON_CONFIG.of_config config in
    { preproc_options = { config; verbose; source_format;
                          exec_preprocs = EXEC_MAP.empty;
                          copybook_lookup_config =
                            Cobol_common.Copybook.lookup_config !libpath
                              ~libexts:!libexts;
                          env };
      parser_options = { config; recovery; verbose; show = !show;
                         exec_scanners = Superbol_preprocs.exec_scanners };
      pretty_verbose = match verbose_on with
        | `Stdout -> Pretty.out
        | `Stderr -> Pretty.error
        | `Stdnul -> Pretty.sink }

  in
  get, args
