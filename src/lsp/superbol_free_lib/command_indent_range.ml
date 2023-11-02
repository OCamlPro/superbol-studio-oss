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

open Ezcmd.V2
open EZCMD.TYPES
open Cobol_indent

open Common_args

let action
  { preproc_options = { source_format; config; _ }; _ } ~filename ~range
=
  let module Config = (val config) in
  let project = Project.for_ ~filename in
  let indent_config = Some (Cobol_indent.config project.config.indent_config) in
  let contents = Ez_file.V1.EzFile.read_file filename in
  indent_range_str ~source_format ~filename ~contents ~range ~indent_config
    ~dialect:Config.dialect |> Fmt.pr "%s"

let cmd =
  let file = ref "" in
  let start_line = ref "" in
  let end_line = ref "" in
  let direct_args = [
    [], Arg.Anon (0, fun f -> file := f), EZCMD.info ~docv:"FILE" "file to check the indentation";
    [], Arg.Anon (1, fun f -> start_line := f), EZCMD.info ~docv:"RANGE_START" "start line of range";
    [], Arg.Anon (2, fun f -> end_line := f), EZCMD.info ~docv:"RANGE_END" "end line of range";
  ] in
  let common, common_args = Common_args.get () in
  let args = direct_args @ common_args in
  let range start_line end_line =
    let open Cobol_indent.Type in
    let start_line = !start_line |> Int32.of_string |> Int32.to_int in
    let end_line = !end_line |> Int32.of_string |> Int32.to_int in
    Some {start_line; end_line}
  in
  EZCMD.sub
    "indent range"
    (fun () ->
       let common = common () in
       action
         common
         ~filename:!file
         ~range:(range start_line end_line)
         )
    ~args
    ~doc: "Indentation range"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
