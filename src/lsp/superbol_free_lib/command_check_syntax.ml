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

let action { platform; preproc_options; parser_options; pretty_progress; _ }
    ~ppf files =
  List.iter begin fun filename ->
    pretty_progress "Checking@ `%s'" filename;
    Cobol_preproc.Input.from ~filename ~f:begin fun input ->
      Cobol_preproc.preprocessor ~options:preproc_options input |>
      Cobol_parser.parse_simple ~options:parser_options
    end ~platform |>
    Cobol_parser.Outputs.sink_result ~platform ~ppf
  end files

let cmd =
  let ppf = ref Fmt.stderr in
  let common, common_args = Common_args.get () in
  let input_files, input_args =
    Input_files.get () ~docv:"FILE" ~doc:"Cobol file to check"
      ~walk:false in

  EZCMD.sub
    "check syntax"          (* add the corresponding line in Main.subcommands *)
    (fun () ->
       let common = common () in
       action common ~ppf:!ppf @@
       input_files ~common ~setup:Project_commands.default_setup)
    ~args:(common_args @ input_args @ [
        [ "stdout" ], Arg.Unit (fun () -> ppf := Fmt.stdout),
        EZCMD.info "Output errors/warnings to stdout";
      ])
    ~doc: "Check the syntax of a Cobol file"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Preprocess and parse each FILE with the options given on the \
            command line only, and report diagnostics.  Use $(b,superbol cache \
            analyze) to follow the configuration of each file's project.";
      ];
    ]
