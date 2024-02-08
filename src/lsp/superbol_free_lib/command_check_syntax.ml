(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(** `parse` command *)

open Ezcmd.V2
open EZCMD.TYPES

open Common_args

let action { preproc_options; parser_options } files =
  let parse input =
    input |>
    Cobol_preproc.preprocessor ~options:preproc_options |>
    Cobol_parser.parse_simple ~options:parser_options
  in
  List.to_seq files |>
  Seq.map begin fun filename ->
    Pretty.out "@[Considering@ `%s'@]@." filename;
    Cobol_preproc.Input.from ~filename ~f:parse
  end

let cmd =
  let files = ref [] in
    let common, common_args = Common_args.get () in

  EZCMD.sub
    "check syntax"            (* add the corresponding line in Main.subcommands *)
    begin fun () ->
      action (common ()) !files |>
      Seq.iter Cobol_common.Diagnostics.sink_result
    end
    ~args:(common_args @ [
        [],
        Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILE" "Cobol file to check";
      ])
    ~doc: "Check the syntax of a Cobol file"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
