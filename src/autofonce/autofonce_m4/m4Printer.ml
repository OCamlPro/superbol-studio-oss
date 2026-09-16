(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open M4Types

let string_of_token = function
  | SHELL s -> Printf.sprintf "SHELL %S" s
  | IDENT s -> Printf.sprintf "IDENT %S" s
  | ONE_ARG s -> Printf.sprintf "ONE_ARG %S" s
  | FIRST_ARG s -> Printf.sprintf "FIRST_ARG %S" s
  | NEXT_ARG s -> Printf.sprintf "NEXT_ARG %S" s
  | LAST_ARG s -> Printf.sprintf "LAST_ARG %S" s
  | COMMENT s -> Printf.sprintf "COMMENT %S" s
  | EOF -> "EOF"

let string_of_location loc =
  Printf.sprintf "%s:%d:%d" loc.file loc.line loc.char

let string_of_arg arg = arg.arg

let string_of_macro statement =
  match statement.kind with
  | Macro ( ident, args ) ->
      Printf.sprintf "Macro: %s ( %s )" ident
        ( String.concat ", "
            ( List.map string_of_arg args ))
  | Shell shell ->
      Printf.sprintf "Shell: %s" shell
  | Comment comment ->
      Printf.sprintf "Comment: %s" comment

let string_of_block list =
  String.concat "\n" ( List.map string_of_macro list )
