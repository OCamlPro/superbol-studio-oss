(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

type t

type variable_information =
  { length : int;
    vartype : int;
    scale : int;
    flags : int;
    ind_addr : int
  }

val transform :
  (Types.loc option * Types.statements) list ->
  (string * t)

val find_opt : t -> string -> variable_information option
