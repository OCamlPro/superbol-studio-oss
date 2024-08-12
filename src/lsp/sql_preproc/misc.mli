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

val error : ?loc:Types.loc -> ('a, unit, string, 'b) format4 -> 'a

val warning : ?loc:Types.loc -> ('a, unit, string, unit) format4 -> 'a

val loc_of_edit : filename:string -> Cobol_indent.Types.token_descr -> Types.loc

val string_of_token : Cobol_indent.Types.token -> string

val add_dot : with_dot:bool -> Buffer.t -> unit

val resolve_copy : config:Types.config -> string -> string

val extract_cob_var_query : Sql_ast.sql_query -> Sql_ast.cobol_var list

val extract_cob_var_select_option_list :
  Sql_ast.sql_select_option list -> Sql_ast.cobol_var list

val extract_cob_var_select : Sql_ast.sql_select -> Sql_ast.cobol_var list

val extract_filename : string -> string

val replace_colon_words : string -> string
