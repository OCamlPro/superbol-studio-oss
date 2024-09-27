(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

type graph = {
  name: string;
  string_repr_dot: string;
  string_repr_d3: string;
  nodes_pos: (int * Cobol_ptree.srcloc) list
}

val make
  : options:Cfg_options.t
  -> name:string
  -> Cobol_typeck.Outputs.t
  -> graph

val possible_cfgs_of_doc
  : Cobol_typeck.Outputs.t
  -> string list
