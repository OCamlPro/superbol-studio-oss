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
  graph_name: string option;
  hide_unreachable: bool;
  collapse_fallthru: bool;
  shatter_hubs: int option;
  transformation: transformation option;
  hidden_nodes: int list;
  split_nodes: int list;
}

let create
    ?(graph_name=None)
    ?(hide_unreachable=false)
    ?(collapse_fallthru=false)
    ?(shatter_hubs=None)
    ?(transformation=None)
    ?(hidden_nodes=[])
    ?(split_nodes=[])
    () =
  {
    hide_unreachable;
    collapse_fallthru;
    graph_name;
    shatter_hubs;
    transformation;
    hidden_nodes;
    split_nodes;
  }

