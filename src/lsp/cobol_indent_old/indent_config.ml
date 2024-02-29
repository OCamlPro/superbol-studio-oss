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

type t = indent_config

let of_list xs =
  let table = Hashtbl.create 16 in
  List.iter (fun (a, b) -> Hashtbl.add table a b) xs;
  table

(*
TODO:
  Need to be rewritten,
  like ocp-indent, we may let the client to decide if
  it activates some features like alignment of argument
*)

(*default offset table*)
let default = of_list
    [ "DEFAULT", 4; (* DEFAULT offset *) (*Do not remove this one*)
      "DISPLAY", 8;
      "USING", 6;
      "REPLACING_COPY", 10;
      "INTO", 5;
      "REPLACE", 8;
      "SELECT", 7;
      "PARAGRAPH", 4;
      (*if need to indent the nested-program, set "PROC_DIV" to no zero*)
      "PROC_DIV", 0;
      (*if need to show more information about the different hierarchy of code,
        these offsets can be set *)
      "IDENT_DIV", 0;
      "ENV_DIV", 0;
      "DATA_DIV", 0;
      "DECLARATIVES", 0;
      "SECTION", 0 ]

let merge t1 t2 =
  let len = Hashtbl.length t1 + Hashtbl.length t2 in
  let table = Hashtbl.create len in
  Hashtbl.iter (Hashtbl.add table) t1;
  Hashtbl.iter (fun a b -> Hashtbl.replace table a b) t2;
  table

let offset_of_keyword offset_table keyword =
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
