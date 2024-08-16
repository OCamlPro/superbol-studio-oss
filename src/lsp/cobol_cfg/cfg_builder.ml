(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cobol_unit
open Cobol_common.Srcloc.INFIX
open Cobol_common.Srcloc.TYPES
open Cobol_unit.Types
open Cobol_common.Visitor
type qualname = Cobol_ptree.qualname

type unconditional_jumps =
  | Goback
  | Go of qualname

type node = {
  name: qualname;
  conditional_jumps: qualname list;
  unconditional_jumps: unconditional_jumps list;
}

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn

let full_qn' ~cu qn = full_qn ~cu ~&qn


let build_node ~default_name ~cu paragraph =
  let open struct
    type acc = {
      conditionals: qualname list;
      unconditional: unconditional_jumps list;
    }
    let add_unconditional uncond acc =
      { acc with unconditional = uncond :: acc.unconditional }
    let add_conditionals acc qn_to_jump =
      { acc with conditionals = qn_to_jump :: acc.conditionals }
  end in
  let { conditionals; unconditional } =
    Visitor.fold_procedure_paragraph'
      object
        inherit [acc] Visitor.folder
        method! fold_goback' _ acc = skip @@ add_unconditional Goback acc
        method! fold_goto' { payload; _ } acc =
          skip @@
          match payload with
          | GoToEntry _ -> acc (* TODO couldn't find doc *)
          | GoToSimple { targets; depending_on } ->
              let targets = Cobol_common.Basics.NEL.to_list targets in
            if Option.is_none depending_on
            then
              add_unconditional (Go (full_qn' ~cu @@ List.hd targets)) acc
            else
              targets
              |> List.map (full_qn' ~cu)
              |> List.fold_left add_conditionals acc
        method! fold_perform_target' { payload; _ } acc =
          skip @@
          let { payload = start; _ } = payload.perform_target.procedure_start in
          (* TODO: check that where we jump has no unconditional_jumps, /!\ cycle` *)
          add_conditionals acc (full_qn ~cu start)
      end
      paragraph {conditionals = []; unconditional = [] }
  in
  let name = Option.fold
      ~none:default_name
      ~some:(full_qn' ~cu)
      ~&paragraph.paragraph_name
  in {
      name;
      conditional_jumps = conditionals;
      unconditional_jumps = unconditional;
    }

module Node = struct
  type t = node
  let compare node other =
    Cobol_ptree.compare_qualname node.name other.name
  let hash node =
    Hashtbl.hash node.name
  let equal node other =
    Cobol_ptree.compare_qualname node.name other.name == 0
end

type edge =
  | Default
  | Conditional
  | Unconditional

module Edge = struct
   type t = edge
   let compare = Stdlib.compare
   let equal = (=)
   let default = Default
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

module Qmap = Map.Make(struct
    type t = qualname
    let compare = Cobol_ptree.compare_qualname
  end)

let rec build_edges ~vertexes g = function
  | ({ conditional_jumps; unconditional_jumps; _ } as current)::next::tl ->
    let g = List.fold_left begin fun g jump_to ->
        let next = Qmap.find jump_to vertexes in
        Cfg.add_edge_e g (current, Conditional, next)
      end g conditional_jumps in
    begin match unconditional_jumps with
      | [] ->
        build_edges ~vertexes (Cfg.add_edge g current next) (next::tl)
      | _ ->
        let g = List.fold_left begin fun g -> function
            | Goback -> g
            | Go jump_to ->
              let next = Qmap.find jump_to vertexes in
              Cfg.add_edge_e g (current, Unconditional, next)
          end g unconditional_jumps in
        build_edges ~vertexes g (next::tl)
    end
  | [{ conditional_jumps; unconditional_jumps; _ } as current] ->
    let g = List.fold_left begin fun g jump_to ->
        let next = Qmap.find jump_to vertexes in
        Cfg.add_edge_e g (current, Conditional, next)
      end g conditional_jumps in
    begin match unconditional_jumps with
      | [] -> g
      | _ ->
        List.fold_left begin fun g -> function
          | Goback -> g
          | Go jump_to ->
            let next = Qmap.find jump_to vertexes in
            Cfg.add_edge_e g (current, Unconditional, next)
        end g unconditional_jumps
    end
  | [] -> g

let cfg_of ~(cu: cobol_unit) =
  let default_name = Cobol_ptree.Name cu.unit_name in
  let nodes = List.fold_left begin fun acc block ->
      match block with
      | Paragraph para ->
        build_node ~default_name ~cu para :: acc
      | Section { payload = { section_paragraphs; _ }; _ } ->
        List.fold_left begin fun acc p ->
          build_node ~default_name ~cu p :: acc
        end acc section_paragraphs.list
    end [] cu.unit_procedure.list |> List.rev
  in
  let g, vertexes = List.fold_left begin fun (g, vertexes) node ->
      Cfg.add_vertex g node,
      Qmap.add node.name node vertexes
    end (Cfg.empty, Qmap.empty) nodes
  in build_edges ~vertexes g nodes

module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes (_,s,_) =
      match s with
      | Default -> [`Style `Dotted]
      | Conditional -> [`Style `Dashed]
      | Unconditional -> [`Style `Solid]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes { unconditional_jumps; _ } =
      if List.exists ((=) Goback) unconditional_jumps
      then [`Style `Bold]
      else []
    let default_vertex_attributes _ = [`Shape `Box]
    let graph_attributes _ = []
    let vertex_name { name = qn; _} =
      Pretty.to_string "\"%a\""Cobol_ptree.pp_qualname qn
      |> Str.global_replace (Str.regexp "\n") " "
  end)

let string_of g =
  Pretty.to_string "%a" Dot.fprint_graph g

let make (checked_doc: Cobol_typeck.Outputs.t) =
  try
  let graphs = Cobol_unit.Collections.SET.fold
      begin fun { payload = cu; _ } acc ->
        acc ^ "\n" ^ (string_of @@ cfg_of ~cu)
      end checked_doc.group ""
  in
  graphs
  with _ -> "no graph failed"

(*
List of node (sections & paragraphs)
Visitor over procedure
  - perform
  - go to
  - output_or_giving
  - input_or_using
  - alter
  - resume
  - declaratives
  - debug_target
  - if else
  - evaluate

  paragraph lié au suivant
  section lié au suivant

pp_dot_format => Graph.Graphviz.Dot
*)
