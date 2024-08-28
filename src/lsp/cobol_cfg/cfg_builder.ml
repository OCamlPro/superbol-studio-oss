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
module NEL = Cobol_common.Basics.NEL

type qualname = Cobol_ptree.qualname

type unconditional_jumps =
  | Goback
  | Go of qualname

type node = {
  num: int;
  name: qualname;
  mutable procedures: qualname NEL.t;
  loc: srcloc;
  conditional_jumps: qualname list;
  unconditional_jumps: unconditional_jumps list;
}

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn

let full_qn' ~cu qn = full_qn ~cu ~&qn

let id = ref 0

let build_node ~default_name ~cu paragraph =
  let open struct
    type acc = {
      conditionals: qualname list;
      unconditional: unconditional_jumps list;
    }
    let init = {conditionals = []; unconditional = [] }
    let add_unconditional uncond acc =
      { acc with unconditional = uncond :: acc.unconditional }
    let add_conditionals acc qn_to_jump =
      { acc with conditionals = qn_to_jump :: acc.conditionals }
  end in
  let { conditionals; unconditional } =
    Visitor.fold_procedure_paragraph'
      object (v)
        inherit [acc] Visitor.folder
        method! fold_goback' _ acc = skip @@ add_unconditional Goback acc
        method! fold_statement' _ ({ unconditional; _ } as acc) =
          match unconditional with
          | [] -> do_children acc
          | _ -> skip acc

        method! fold_if' { payload = { then_branch; else_branch; _ }; _ } acc =
          let { conditionals; unconditional } =
            Cobol_ptree.Visitor.fold_statements v then_branch init in
          let { conditionals = else_cond; unconditional = else_uncond } =
            Cobol_ptree.Visitor.fold_statements v else_branch init in
          skip {
            conditionals = acc.conditionals @ conditionals @ else_cond;
            unconditional = acc.unconditional @ unconditional @ else_uncond;
          }

        method! fold_goto' { payload; _ } acc =
          skip @@
          match payload with
          | GoToEntry _ -> acc (* TODO couldn't find doc *)
          | GoToSimple { target } ->
            add_unconditional (Go (full_qn' ~cu target)) acc
          | GoToDepending { targets; _ } ->
            Cobol_common.Basics.NEL.to_list targets
            |> List.map (full_qn' ~cu)
            |> List.fold_left add_conditionals acc
        method! fold_perform_target' { payload; _ } acc =
          skip @@
          let { payload = start; _ } = payload.perform_target.procedure_start in
          (* TODO: check that where we jump has no unconditional_jumps, /!\ cycle` *)
          add_conditionals acc (full_qn ~cu start)
      end
      paragraph init
  in
  id:=!id+1;
  let name, loc = match ~&paragraph.paragraph_name with
    | None -> default_name, ~@paragraph
    | Some name -> full_qn' ~cu name, ~@name
  in {
    num = !id;
    name;
    procedures = NEL.One name;
    loc;
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
  | FallThrough
  | Conditional
  | Unconditional

module Edge = struct
   type t = edge
   let compare = Stdlib.compare
   let equal = (=)
   let default = FallThrough
   let to_string = function
     | FallThrough -> "d"
     | Conditional -> "c"
     | Unconditional -> "u"
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

let vertex_name_quoted { procedures; _ } =
  Pretty.to_string "%a"
    (NEL.pp ~fopen:"\"" ~fclose:"\"" ~fsep:"\n" Cobol_ptree.pp_qualname)
    (NEL.rev procedures)

let vertex_name { name = qn; _ } =
  Pretty.to_string "%a" Cobol_ptree.pp_qualname qn
  |> Str.global_replace (Str.regexp "\n") " "

module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes (_,s,_) =
      match s with
      | FallThrough -> [`Style `Dotted]
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
    let vertex_name = vertex_name_quoted
  end)

let string_of g =
  Pretty.to_string "%a" Dot.fprint_graph g

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
        (* build_edges ~vertexes g (next::tl) *)
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
  in
  let g = build_edges ~vertexes g nodes in
  Cfg.fold_vertex begin fun n cfg ->
    match Cfg.pred_e cfg n with
    | [(pred, FallThrough, _)] ->
      Lsp_io.log_debug "before node %s (pred is %s):\n%s\n" (vertex_name n) (vertex_name pred) @@ string_of cfg;
      let cfg = Cfg.fold_succ_e begin fun (_, e, next) cfg ->
          Cfg.add_edge_e cfg (pred, e, next)
        end cfg n cfg in
      Lsp_io.log_debug "after edge adding:\n%s\n" @@ string_of cfg;
      pred.procedures <- NEL.(n.procedures @ pred.procedures);
      Cfg.remove_vertex cfg n
    | _ -> cfg
  end g g
(* g *)

type graph = {
  string_repr: string;
  nodes_pos: (string * srcloc) list
}

let make_dot ({ group; _ }: Cobol_typeck.Outputs.t) =
  Cobol_unit.Collections.SET.fold
    begin fun { payload = cu; _ } acc ->
      let cfg = cfg_of ~cu in
      let nodes_pos = Cfg.fold_vertex begin fun n acc ->
          (vertex_name n, n.loc)::acc
        end cfg [] in
      {
        string_repr = string_of cfg;
        nodes_pos;
      } :: acc
    end group []

let make_d3 ({ group; _ }: Cobol_typeck.Outputs.t) =
  Cobol_unit.Collections.SET.fold
    begin fun { payload = cu; _ } acc ->
      let cfg = cfg_of ~cu in
      let cfg_edges = Cfg.fold_edges_e
          begin fun (n1, e, n2) links ->
            links
            ^ Pretty.to_string "{source: '%s', target:'%s', type:'%s'},"
              (vertex_name n1) (vertex_name n2) (Edge.to_string e)
          end cfg "[" ^ "]" in
      let cfg_nodes = Cfg.fold_vertex
          begin fun n nodes ->
            nodes
            ^ Pretty.to_string "{id:'%s'}," (vertex_name n)
          end cfg "[" ^ "]" in
      {
        string_repr = Pretty.to_string "{links:%s, nodes:%s}" cfg_edges cfg_nodes;
        nodes_pos = Cfg.fold_vertex begin fun n acc ->
            (vertex_name n, n.loc)::acc
          end cfg []
      } :: acc
    end group []

let make ?(d3=false) (checked_doc: Cobol_typeck.Outputs.t) =
  if d3
  then make_d3 checked_doc
  else make_dot checked_doc

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
  - exit

  paragraph lié au suivant
  section lié au suivant

pp_dot_format => Graph.Graphviz.Dot
*)
