(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cfg_jumps

type node_type =
  | External of string
  | Entry of [`Point | `Paragraph | `Section of string | `Statement of string]
  | Normal of string * string (* fullname * display_name *)
  | Collapsed of string Cobol_common.Basics.NEL.t
  | Split of string

type node = {
  id: int;
  section_name: string;
  loc: Cobol_common.srcloc option;
  typ: node_type;
  jumps: Jumps.t;
  will_fallthru: bool;
  terminal: bool; (* unused atm *)
}

module Node: sig
  type t = node

  val compare : node -> node -> int
  val hash : node -> int
  val equal : node -> node -> bool
end

type edge =
  | FallThrough
  | Perform
  | Go

module Edge: sig
  type t = edge

  val compare : 'a -> 'a -> int
  val default : edge
end

module Cfg :
sig
  type t

  module V : sig
    type t = node

    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    type label = t

    val create : label -> t
    val label : t -> label
  end

  type vertex = node

  module E : sig
    type t = vertex * edge * vertex

    val compare : t -> t -> int

    type vertex = node

    val src : t -> vertex
    val dst : t -> vertex

    type label = edge

    val create : vertex -> label -> vertex -> t
    val label : t -> label
  end

  type edge = E.t

  val is_directed : bool
  val is_empty : t -> bool
  val nb_vertex : t -> int
  val nb_edges : t -> int
  val out_degree : t -> vertex -> int
  val in_degree : t -> vertex -> int
  val mem_vertex : t -> vertex -> bool
  val mem_edge : t -> vertex -> vertex -> bool
  val mem_edge_e : t -> edge -> bool
  val find_edge : t -> vertex -> vertex -> edge
  val find_all_edges : t -> vertex -> vertex -> edge list
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list
  val iter_vertex : (vertex -> unit) -> t -> unit
  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges : (vertex -> vertex -> unit) -> t -> unit

  val fold_edges :
    (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_edges_e : (edge -> unit) -> t -> unit
  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  val map_vertex : (vertex -> vertex) -> t -> t
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit

  val fold_succ :
    (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val fold_pred :
    (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val iter_succ_e : (edge -> unit) -> t -> vertex -> unit

  val fold_succ_e :
    (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val iter_pred_e : (edge -> unit) -> t -> vertex -> unit

  val fold_pred_e :
    (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val empty : t
  val add_vertex : t -> vertex -> t
  val remove_vertex : t -> vertex -> t
  val add_edge : t -> vertex -> vertex -> t
  val add_edge_e : t -> edge -> t
  val remove_edge : t -> vertex -> vertex -> t
  val remove_edge_e : t -> edge -> t
end


val make
  : options:Cfg_options.t
  -> name:string
  -> Cobol_typeck.Outputs.t
  -> Cfg.t * Cfg.t

val possible_cfgs_of_doc
  : Cobol_typeck.Outputs.t
  -> string list
