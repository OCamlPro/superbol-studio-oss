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

open Indent_type
open Indent_keywords


(*
TODO:
  Need to be rewritten,
  like ocp-indent, we may let the client to decide if
  it activates some features like alignment of argument
*)

(*we read the file user_def, and save it into the Hashtable offset_table*)
let rec build_table strlist offset_table =
  let help str1 =
    let str2 = String.split_on_char ';' str1 in
    let name = List.hd str2 in
    let value = List.nth str2 1 in
    name,  Int32.to_int @@ Int32.of_string  value
  in
  match strlist with
  | "" :: _ -> ()
  | str :: stl ->
     let x,y = help str in
     Hashtbl.add offset_table x y;
     build_table stl offset_table
  | _ -> ()

let offset_table = Hashtbl.create 16
(*default offset table*)
let () =
  List.iter
    (fun (a, b) -> Hashtbl.add offset_table a b)
    [ "DEFAULT", 4; (* DEFAULT offset *) (*Do not remove this one*)
      "DISPLAY", 8;
      "USING", 6;
      "REPLACING_COPY", 10;
      "INTO", 5;
      "REPLACE", 8;
      "SELECT", 7;
      "PARAGRAPH", 4;
      (*if need to indent the nested-program, set "PROC_DIV" to no zero*)
      "PROC_DIV", 1;
      (*if need to show more information about the different hierarchy of code,
        these offsets can be set *)
      "IDENT_DIV", 0;
      "ENV_DIV", 0;
      "DATA_DIV", 0;
      "DECLARATIVES", 0;
      "SECTION", 0 ]

let set_config ~indent_config =
  match Ez_file.V1.EzFile.read_file indent_config with
  | str ->
    let strlist = String.split_on_char '\n' str in
    build_table strlist offset_table
  | exception Sys_error _ -> ()

let offset_of_keyword keyword =
  match keyword with
  (*WARNING: these tokens must have offset 0*)
  | COMPILATION_UNIT
  | DATA_DESC
  | THEN | ELSE
  | DUMMY_EXCEPTION
  | ARGUMENT -> 0
  | _ ->
    let str = string_of_keyword keyword in
    match Hashtbl.find_opt offset_table str with
    | Some x -> x
    | None ->
        try Hashtbl.find offset_table "DEFAULT" with Not_found -> 0
