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
  let files = ref [] in
  let ppf = ref Fmt.stderr in
  let in_project = ref false in
  let common, common_args = Common_args.get () in

  EZCMD.sub
    "check syntax"          (* add the corresponding line in Main.subcommands *)
    begin fun () ->
      if !in_project
      then Caching_commands.analyze_n_report (common ()) ~for_lsp:false
             ~ppf:!ppf !files
      else action (common ()) ~ppf:!ppf !files
    end
    ~args:(common_args @ [
        [],
        Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILE" "Cobol file to check";

        [ "stdout" ], Arg.Unit (fun () -> ppf := Fmt.stdout),
        EZCMD.info "Output errors/warnings to stdout";

        [ "project" ], Arg.Set in_project,
        EZCMD.info "Check each file according to the configuration of its \
                    project, reusing and filling the project's cache (see \
                    `superbol cache analyze')";
      ])
    ~doc: "Check the syntax of a Cobol file"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Preprocess and parse each FILE, and report diagnostics.  By default, \
            only the options given on the command line are used.";
        `P "With $(b,--project), each FILE is analyzed as the LSP server would: \
            according to the configuration of the project it belongs to (which \
            options given on the command line override), with typechecking, and \
            leaving out files that appear to be copybooks. Results are looked up \
            in the cache of that project, which also holds those of the LSP \
            server and $(b,superbol cache analyze).  On a miss, results are \
            stored into the cache without the artifacts the LSP server needs, \
            so they are not reused by the latter (use $(b,superbol cache \
            analyze) to fill the cache for the LSP server).";
      ];
    ]
