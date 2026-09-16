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

type location = {
  file : string ;
  line : int ;
  char : int ;
}

exception Error of string * location

type token =
  | SHELL of string
  | IDENT of string
  | ONE_ARG of string
  | FIRST_ARG of string
  | NEXT_ARG of string
  | LAST_ARG of string
  | COMMENT of string
  | EOF

type block = statement list

and kind =
  | Macro of string * arg list
  | Shell of string
  | Comment of string

and statement = {
  kind : kind ;
  loc : location ;
}

and arg = {
  arg : string ;
  arg_loc : location ;
}
