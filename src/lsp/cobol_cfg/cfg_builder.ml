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

module Qualnames = Set.Make(struct
  type t = qualname
  let compare = Cobol_ptree.compare_qualname
end)

module Unconditionals = Set.Make(struct
  type t = unconditional_jumps
  let compare u1 u2 =
    match u1, u2 with
    | Goback, Goback -> 0
    | Go qn1, Go qn2 -> Cobol_ptree.compare_qualname qn1 qn2
    | Go _, Goback -> -1
    | Goback, Go _ -> 1
end)

module Qmap = Map.Make(struct
    type t = qualname
    let compare = Cobol_ptree.compare_qualname
  end)

type node = {
  id: int;
  qid: qualname;
  mutable names: string NEL.t;
  loc: srcloc option;
  entry: bool;
  conditional_jumps: Qualnames.t;
  unconditional_jumps: Unconditionals.t;
}

let qn_to_string qn =
  Pretty.to_string "%a" Cobol_ptree.pp_qualname qn

let qn_equal qn1 qn2 = 0 == Cobol_ptree.compare_qualname qn1 qn2

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn

let full_qn' ~cu qn = full_qn ~cu ~&qn

let node_idx = ref 0

let build_node ~default_name ~cu paragraph =
  let open struct
    type acc = {
      conditionals: Qualnames.t;
      unconditional: Unconditionals.t;
      unreachable: bool;
    }
    let init = {conditionals = Qualnames.empty;
                unconditional = Unconditionals.empty;
                unreachable = false; }
    let add_unconditional uncond acc =
      { acc with unconditional = Unconditionals.add uncond acc.unconditional;
                 unreachable = true; }
    let add_conditionals acc qn_to_jump =
      { acc with conditionals = Qualnames.add qn_to_jump acc.conditionals }
  end in
  let { conditionals; unconditional; unreachable = _ } =
    Visitor.fold_procedure_paragraph'
      object (v)
        inherit [acc] Visitor.folder
        method! fold_goback' _ acc = skip @@ add_unconditional Goback acc
        method! fold_statement' _ ({ unreachable; _ } as acc) =
          if unreachable
          then skip acc
          else do_children acc
        method! fold_if' { payload = { then_branch; else_branch; _ }; _ } acc =
          let { conditionals; unconditional; unreachable } =
            Cobol_ptree.Visitor.fold_statements v then_branch acc in
          let { conditionals = else_cond; unconditional = else_uncond; unreachable = else_unreach } =
            Cobol_ptree.Visitor.fold_statements v else_branch init in
          skip {
            conditionals = Qualnames.union conditionals else_cond;
            unconditional = Unconditionals.union unconditional else_uncond;
            unreachable = unreachable && else_unreach
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
  node_idx:=!node_idx+1;
  let qid, loc = match ~&paragraph.paragraph_name with
    | None -> default_name, ~@paragraph
    | Some qn -> full_qn' ~cu qn, ~@qn in
  let name = qn_to_string qid
  in {
    id = !node_idx;
    qid;
    names = NEL.One name;
    loc = Some loc;
    entry = false;
    conditional_jumps = conditionals;
    unconditional_jumps = unconditional;
  }

module Node = struct
  type t = node
  let compare node other =
    Int.compare node.id other.id
  let hash node =
    Hashtbl.hash node.id
  let equal node other =
    Int.equal node.id other.id
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

let vertex_name_record { names; _ } =
  Pretty.to_string "%a"
    (NEL.pp ~fopen:"{" ~fclose:"}" ~fsep:"|" Fmt.string)
    (NEL.rev names)

let vertex_name { names; _ } =
  Pretty.to_string "%a"
    (NEL.pp ~fopen:"" ~fclose:"" ~fsep:"\n" Fmt.string)
    (NEL.rev names)


let vertex_name_no_newline { names; _ } =
  Pretty.to_string "%a"
    (NEL.pp ~fopen:"" ~fclose:"" ~fsep:";" Fmt.string)
    (NEL.rev names)
  |> Str.global_replace (Str.regexp "\n") " "

module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes (_,s,_) =
      [`Style (match s with
           | FallThrough -> `Dotted
           | Conditional -> `Dashed
           | Unconditional -> `Solid)]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes ({ unconditional_jumps; entry; _ } as n) =
      [`Label (if entry then vertex_name n else vertex_name_record n)]
      @ (if Unconditionals.mem Goback unconditional_jumps
         then [`Style `Bold]
         else [])
      @ (if entry then [`Shape `Doubleoctagon] else [])
    let default_vertex_attributes _ = [`Shape `Record]
    let graph_attributes _ = []
    let vertex_name { id; _ } = string_of_int id
  end)

let string_of g =
  Pretty.to_string "%a" Dot.fprint_graph g

let dummy_node qn =
  node_idx:= !node_idx + 1;
  {
    id = !node_idx;
    qid = qn;
    loc = None;
    entry = false;
    names = NEL.One (qn_to_string qn);
    unconditional_jumps = Unconditionals.empty;
    conditional_jumps = Qualnames.empty;
  }

let qmap_find_or_add cfg qn qmap =
  match Qmap.find_opt qn qmap with
  | None -> let node = dummy_node qn in
    node, Cfg.add_vertex cfg node
  | Some node -> node, cfg

let rec build_edges ~vertexes g nodes =
  let g = match nodes with
    | ({ conditional_jumps; unconditional_jumps; _ } as current)::_ ->
      Qualnames.fold begin fun jump_to g ->
        let next, g = qmap_find_or_add g jump_to vertexes in
        Cfg.add_edge_e g (current, Conditional, next)
      end conditional_jumps g
      |> Unconditionals.fold begin fun uncond g ->
        match uncond with
        | Goback -> g
        | Go jump_to ->
          let next, g = qmap_find_or_add g jump_to vertexes in
          Cfg.add_edge_e g (current, Unconditional, next)
      end unconditional_jumps
    | [] -> g
  in
  match nodes with
  | ({ unconditional_jumps; _ } as current)::next::tl
    when Unconditionals.is_empty unconditional_jumps ->
    build_edges ~vertexes (Cfg.add_edge g current next) (next::tl)
  | _::tl -> build_edges ~vertexes g tl
  | [] -> g

let cfg_of_nodes ~fall_thru_compact nodes =
  let g, vertexes = List.fold_left begin fun (g, vertexes) node ->
      Cfg.add_vertex g node,
      Qmap.add node.qid node vertexes
    end (Cfg.empty, Qmap.empty) nodes
  in
  let g = build_edges ~vertexes g nodes in
  if not fall_thru_compact
  then g
  else Cfg.fold_vertex begin fun n cfg ->
      match Cfg.pred_e cfg n with
      | [(({ entry = false; _ } as pred), FallThrough, _)] ->
        let cfg = Cfg.fold_succ_e begin fun (_, e, next) cfg ->
            if List.exists
                begin fun succ -> qn_equal succ.qid next.qid end
                (Cfg.succ cfg pred)
            then cfg
            else Cfg.add_edge_e cfg (pred, e, next)
          end cfg n cfg in
        pred.names <- NEL.(n.names @ pred.names);
        Cfg.remove_vertex cfg n
      | _ -> cfg
    end g g

let cfg_of ~(cu: cobol_unit) =
  node_idx := 0;
  let default_name = Cobol_ptree.Name cu.unit_name in
  let nodes = List.fold_left begin fun acc block ->
      match block with
      | Paragraph para ->
        build_node ~default_name ~cu para :: acc
      | Section { payload = { section_paragraphs; _ }; _ } ->
        List.fold_left begin fun acc p ->
          build_node ~default_name ~cu p :: acc
        end acc section_paragraphs.list
    end [] cu.unit_procedure.list
  in
  List.rev nodes
  |> begin function (* adding entry point if not already present *)
    | ({ qid; _ } as hd )::tl
      when qn_equal qid default_name ->
        { hd with entry = true; names = NEL.One "Entry\nparagraph" }::tl
    | l ->
        { (dummy_node default_name) with entry = true; names = NEL.One "Entry\npoint" } :: l
  end
  |> cfg_of_nodes

let cfg_of_section ~cu ({ section_paragraphs; section_name }: procedure_section) =
  node_idx := 0;
  let default_name = ~&section_name in
  let nodes =
    List.fold_left begin fun acc p ->
      build_node ~default_name ~cu p :: acc
    end [] section_paragraphs.list
    |> List.rev
  in cfg_of_nodes nodes

type graph = {
  name: string;
  string_repr: string;
  nodes_pos: (int * srcloc) list
}

let nodes_pos cfg =
  Cfg.fold_vertex begin fun n acc ->
    match n.loc with
    | None -> acc
    | Some loc -> (n.id, loc)::acc
  end cfg []

let make_dot ({ group; _ }: Cobol_typeck.Outputs.t) =
  Cobol_unit.Collections.SET.fold
    begin fun { payload = cu; _ } acc ->
      let section_graphs = List.filter_map begin function
          | Paragraph _ -> None
          | Section sec -> Some (
              let name =
                Pretty.to_string "%a" Cobol_ptree.pp_qualname' ~&sec.section_name in
              let cfg = cfg_of_section ~fall_thru_compact:true ~cu ~&sec in
              let nodes_pos = nodes_pos cfg in {
                name;
                string_repr = string_of cfg;
                nodes_pos;
              })
        end cu.unit_procedure.list in
      let cfg = cfg_of ~fall_thru_compact:true ~cu in
      {
        name = (~&)cu.unit_name;
        string_repr = string_of cfg;
        nodes_pos = nodes_pos cfg;
      } :: section_graphs @ acc
    end group []

let make_d3 ({ group; _ }: Cobol_typeck.Outputs.t) =
  Cobol_unit.Collections.SET.fold
    begin fun { payload = cu; _ } acc ->
      let cfg = cfg_of ~fall_thru_compact:false ~cu in
      let cfg_edges = Cfg.fold_edges_e
          begin fun (n1, e, n2) links ->
            links
            ^ Pretty.to_string "{source: %d, target:%d, type:'%s'},"
              n1.id n2.id (Edge.to_string e)
          end cfg "[" ^ "]" in
      let cfg_nodes = Cfg.fold_vertex
          begin fun n nodes ->
            nodes
            ^ Pretty.to_string "{id:%d, name: '%s'}," n.id (vertex_name_no_newline n)
          end cfg "[" ^ "]" in
      {
        name = (~&)cu.unit_name;
        string_repr = Pretty.to_string "{links:%s, nodes:%s}" cfg_edges cfg_nodes;
        nodes_pos = nodes_pos cfg
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
