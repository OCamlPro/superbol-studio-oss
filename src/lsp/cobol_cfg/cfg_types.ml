(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cobol_common.Srcloc.TYPES
open Cfg_jumps
module NEL = Cobol_common.Basics.NEL

type node_type =
  | External of string
  | Entry of [`Point | `Paragraph | `Section of string | `Statement of string]
  | Normal of string * string (* fullname * display_name *)
  | Collapsed of string NEL.t
  | Split of string

type node = {
  id: int;
  section_name: string;
  loc: srcloc option;
  typ: node_type;
  jumps: Jumps.t;
  will_fallthru: bool;
  terminal: bool; (* unused atm *)
}

type edge =
  | FallThrough
  | Perform
  | Go

module Node = struct
  type t = node
  let compare node other = Int.compare node.id other.id
  let hash node = Hashtbl.hash node.id
  let equal node other = Int.equal node.id other.id
end

module Edge = struct
  type t = edge
  let compare = Stdlib.compare
  let default = FallThrough
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)
