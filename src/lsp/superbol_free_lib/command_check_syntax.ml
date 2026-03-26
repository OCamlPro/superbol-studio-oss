(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(** `check syntax` command *)

open Ezcmd.V2
open EZCMD.TYPES

open Common_args

let action { platform; preproc_options; parser_options; pretty_verbose }
    ~ppf files =
  let parse input =
    input |>
    Cobol_preproc.preprocessor ~options:preproc_options |>
    Cobol_parser.parse_simple ~options:parser_options
  in
  files |>
  List.iter begin fun filename ->
    pretty_verbose "@[Checking@ `%s'@]@." filename;
    Cobol_parser.Outputs.sink_result ~platform ~ppf @@
    Cobol_preproc.Input.from ~filename ~f:parse ~platform;
  end

let cmd =
  let files = ref [] in
  let ppf = ref Fmt.stderr in
  let common, common_args = Common_args.get () in

  EZCMD.sub
    "check syntax"            (* add the corresponding line in Main.subcommands *)
    begin fun () ->
      action (common ()) ~ppf:!ppf !files
    end
    ~args:(common_args @ [
      [],
      Arg.Anons (fun list -> files := list),
      EZCMD.info ~docv:"FILE" "Cobol file to check";

      [ "stdout" ], Arg.Unit (fun () -> ppf := Fmt.stdout),
      EZCMD.info "Output errors/warnings to stdout";
    ])
    ~doc: "Check the syntax of a Cobol file"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
