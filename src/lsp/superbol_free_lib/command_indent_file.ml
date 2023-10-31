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

let action { preproc_options = { source_format; config; _ } ; _ } files =
  let module Config = (val config) in
  let project = Project_config.load_project () in
  let indent_config = Some (Cobol_indent.config project.config.indent_config) in
  List.to_seq files
  |> Seq.map (fun file ->
    let contents = Ez_file.V1.EzFile.read_file file in
    indent_range_str
      ~source_format ~filename:file ~contents ~indent_config ~range:None
      ~dialect:Config.dialect |> Fmt.pr "%s")

let cmd =
  let files = ref [] in
  let common, common_args = Common_args.get () in
  let args = common_args  in
  EZCMD.sub
    "indent file"
    (fun () ->
       let common = common () in
       Seq.iter ignore @@ action common !files)
    ~args:(args @ [
        [],
      Arg.Anons (fun list -> files := list),
      EZCMD.info ~docv:"FILES" "Cobol files to indent"])
    ~doc: "Indentation"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
