(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

type error =
  | Failure of string

exception Error of error

type loc = { filename : string ;
             line : int ;
             char : int }

(* These statements show how we could keep information and modify the
   corresponding places in the code *)

type statements =
  | PROCEDURE_DIVISION_DOT of { end_loc : loc }
  | WORKING_STORAGE of { defined: bool }
  | LINKAGE_SECTION of { defined: bool }
  | EXEC_SQL of { end_loc : loc ;
                   with_dot : bool ;
                   tokens : Sql_ast.esql_instuction ;
                 }
  | BEGIN_PROCEDURE_DIVISION of { enabled : bool ref }
  | END_PROCEDURE_DIVISION
  | COPY of { end_loc : loc ; filename : string ; contents : string }
  | IS_SQLVAR of { end_loc : loc }


type handle = {
  mutable handle_abend : string ;
}

type gen_context = {
  b : Buffer.t ;
  main_filename : string ;
}

type config = {
  scanner_config: Cobol_indent.Types.config ;
  sql_in_copybooks : bool;
  copy_path : ( string * string StringMap.t) list Lazy.t ;
  copy_exts : string list ;
  verbosity : int ;
}
