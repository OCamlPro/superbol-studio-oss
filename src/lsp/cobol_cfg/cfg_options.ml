(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

type transformation =
  | Descendents of int
  | Neighborhood of int

type t = {
  hide_unreachable: bool;
  collapse_fallthru: bool;
  in_degree_upper_limit: int option;
  transformation: transformation option;
  hidden_nodes: int list; (* id list *)
  split_nodes: int list;  (* id list *)
}

