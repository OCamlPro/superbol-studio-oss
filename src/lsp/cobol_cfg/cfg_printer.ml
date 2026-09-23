(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2026 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cfg_types

module CFG4DOT = struct
  include CFG
  let edge_attributes (_,s,_) =
    [`Style (match s with
         | FallThrough -> `Dotted
         | Perform -> `Dashed
         | Go -> `Solid)]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_name_record names =
    Pretty.to_string "%a"
      (Cobol_common.Basics.NEL.pp ~fopen:"{" ~fclose:"}" ~fsep:"|" Fmt.string)
      names
  let vertex_attributes { typ; _ } =
    let label, attributes =
      match typ with
      | Entry (`Section name) -> name, [`Shape `Doubleoctagon]
      | Entry (`Statement name) -> name, [`Shape `Doubleoctagon]
      | Entry `Point -> "Entry\\npoint", [`Shape `Doubleoctagon]
      | Entry `Paragraph -> "Entry\\nparagraph", [`Shape `Doubleoctagon]
      | External name -> name, [`Shape `Plaintext]
      | Split name -> name, [`Style `Dashed]
      | Normal (_, name) -> name, []
      | Collapsed names -> vertex_name_record names, [`Shape `Record]
    in `Label label :: attributes
  let default_vertex_attributes _ = [`Shape `Box]
  let graph_attributes _ = []
  let vertex_name { id; _ } = string_of_int id
end

module DOT = Graph.Graphviz.Dot (CFG4DOT)

let pp_cfg_dot ppf (cfg: CFG.t) =
  DOT.fprint_graph ppf cfg
