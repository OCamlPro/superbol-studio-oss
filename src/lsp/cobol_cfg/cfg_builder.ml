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

module Options = struct
  type t = {
    graph_name: string option;
    hide_unreachable: bool;
    collapse_fallthru: bool;
    shatter_hubs: int option;
  }

  let create
  ?(graph_name=None)
  ?(hide_unreachable=false)
  ?(collapse_fallthru=true)
  ?(shatter_hubs=None)
  ()
  = { hide_unreachable; collapse_fallthru; graph_name; shatter_hubs }

let from_yojson_assoc o =
  let graph_name =
    try Some (List.assoc "graph_name" o |> Yojson.Safe.Util.to_string)
    with Not_found -> None in
  let hide_unreachable =
    try Some (List.assoc "hide_unreachable" o |> Yojson.Safe.Util.to_bool)
    with Not_found -> None in
  let collapse_fallthru =
    try Some (List.assoc "collapse_fallthru" o |> Yojson.Safe.Util.to_bool)
    with Not_found -> None in
  let shatter_hubs =
    try Some (List.assoc "shatter_hubs" o |> Yojson.Safe.Util.to_int)
    with Not_found -> None in
  create ~graph_name ?hide_unreachable ?collapse_fallthru ~shatter_hubs ()
end

type qualname = Cobol_ptree.qualname

type jumps =
  | Goback
  | Go of qualname
  | Conditional of qualname (* includes perform, go ... depending *)

module Qualnames = Set.Make(struct
  type t = qualname
  let compare = Cobol_ptree.compare_qualname
end)

module Jumps = struct
  include Set.Make(struct
      type t = jumps
      let compare j1 j2 =
        let to_int = function
          | Goback -> 0
          | Go _ -> 1
          | Conditional _ -> 2 in
        match j1, j2 with
        | Go qn1, Go qn2 -> Cobol_ptree.compare_qualname qn1 qn2
        | Conditional qn1, Conditional qn2 -> Cobol_ptree.compare_qualname qn1 qn2
        | _ -> to_int j2 - to_int j1
    end)
  let only_conditional : t -> bool =
    for_all begin function
      | Conditional _ -> true | _ -> false
    end
end

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
  jumps: Jumps.t;
  is_external: bool;
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
      jumps: Jumps.t;
      unreachable: bool;
    }
    let init = { jumps = Jumps.empty;
                 unreachable = false; }
    let add_unconditional uncond acc =
      { jumps = Jumps.add uncond acc.jumps;
        unreachable = true; }
    let add_conditionals acc qn_to_jump =
      { acc with jumps = Jumps.add (Conditional qn_to_jump) acc.jumps }
  end in
  let { jumps; unreachable = _ } =
    Visitor.fold_procedure_paragraph'
      object (v)
        inherit [acc] Visitor.folder
        method! fold_goback' _ acc = skip @@ add_unconditional Goback acc
        method! fold_statement' _ ({ unreachable; _ } as acc) =
          if unreachable
          then skip acc
          else do_children acc
        method! fold_if' { payload = { then_branch; else_branch; _ }; _ } acc =
          let {  jumps; unreachable } =
            Cobol_ptree.Visitor.fold_statements v then_branch acc in
          let { jumps = else_j; unreachable = else_unreach } =
            Cobol_ptree.Visitor.fold_statements v else_branch init in
          skip {
            jumps = Jumps.union jumps else_j;
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
    jumps;
    is_external = false;
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

  (* Graph.Graphviz.DotAttributes *)
module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes (_,s,_) =
      [`Style (match s with
           | FallThrough -> `Dotted
           | Conditional -> `Dashed
           | Unconditional -> `Solid)]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes ({ entry; is_external; _ } as n) =
      [`Label (if entry then vertex_name n else vertex_name_record n)]
      @ (if entry
      then [`Shape `Doubleoctagon]
      else if is_external
      then [`Shape `Plaintext]
      else [])
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
    jumps = Jumps.empty;
    is_external = true;
  }

let clone_node node =
  node_idx:= !node_idx + 1;
  { node with id = !node_idx; }

let qmap_find_or_add qmap qn =
  match Qmap.find_opt qn qmap with
  | None -> let node = dummy_node qn in
    (* qmap, node *)
    Qmap.add qn node qmap, node
  | Some node -> qmap, node

let rec build_edges ~vertexes g nodes =
  let g, vertexes = match nodes with
    | ({ jumps; _ } as current)::_ ->
      Jumps.fold begin fun uncond (g, vertexes) ->
        match uncond with
        | Goback -> g, vertexes
        | Go jump_to ->
          let vertexes, next = qmap_find_or_add vertexes jump_to in
          Cfg.add_edge_e g (current, Unconditional, next),
          vertexes
        | Conditional jump_to ->
          let vertexes, next = qmap_find_or_add vertexes jump_to in
          Cfg.add_edge_e g (current, Conditional, next),
          vertexes
      end jumps (g, vertexes)
    | [] -> g, vertexes
  in
  match nodes with
  | ({ jumps; _ } as current)::next::tl
    when Jumps.only_conditional jumps ->
    build_edges ~vertexes (Cfg.add_edge g current next) (next::tl)
  | _::tl -> build_edges ~vertexes g tl
  | [] -> g


let do_collapse_fallthru g =
  Cfg.fold_vertex begin fun n cfg ->
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

let do_hide_unreachable g =
  let rec aux cfg =
    let did_remove, cfg =
      Cfg.fold_vertex begin fun n (did_remove, cfg) ->
        if Cfg.in_degree cfg n <= 0 && not n.entry
        then true, Cfg.remove_vertex cfg n
        else did_remove, cfg
      end cfg (false, cfg)
    in
    if did_remove then aux cfg else cfg
  in aux g

let do_shatter_hubs ?(limit=20) g =
  Cfg.fold_vertex begin fun n cfg ->
    if Cfg.in_degree cfg n >= limit && not n.entry
    then begin
      Cfg.fold_pred_e begin fun edge cfg ->
        let cfg = Cfg.remove_edge_e cfg edge in
        let n_clone = clone_node n in
        let (pred, edge, _) = edge in
        let cfg = Cfg.add_edge_e cfg (pred, edge, n_clone) in
        cfg
      end cfg n cfg
    end
    else cfg
  end g g

let cfg_of_nodes ~(options: Options.t) nodes =
  let g, vertexes = List.fold_left begin fun (g, vertexes) node ->
      Cfg.add_vertex g node,
      Qmap.add node.qid node vertexes
    end (Cfg.empty, Qmap.empty) nodes
  in
  build_edges ~vertexes g nodes
  |> (if options.collapse_fallthru then do_collapse_fallthru else Fun.id)
  |> (match options.shatter_hubs with
      | Some limit -> do_shatter_hubs ~limit
      | _ -> Fun.id)
  |> (if options.hide_unreachable then do_hide_unreachable else Fun.id)

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
    |> List.rev in
  let nodes = match nodes with
    | entry::tl -> { entry with entry = true }::tl
    | [] -> []
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

let make_dot ~(options: Options.t) ({ group; _ }: Cobol_typeck.Outputs.t) =
  let is_to_include : string -> bool =
    match options.graph_name with
    | None -> Fun.const true
    | Some name -> String.equal name in
  Cobol_unit.Collections.SET.fold
    begin fun { payload = cu; _ } acc ->
      let section_graphs = List.filter_map begin function
          | Paragraph _ -> None
          | Section sec ->
            let name = Pretty.to_string "%a (%s)"
                Cobol_ptree.pp_qualname' ~&sec.section_name
                ((~&) cu.unit_name) in
            if not (is_to_include name)
            then None
            else Some (
                let cfg = cfg_of_section ~options ~cu ~&sec in
                let nodes_pos = nodes_pos cfg in {
                  name;
                  string_repr = string_of cfg;
                  nodes_pos;
                })
        end cu.unit_procedure.list in
      let cu_graph =
        if is_to_include ((~&) cu.unit_name)
        then
          let cfg = cfg_of ~options ~cu in
          [{
            name = (~&)cu.unit_name;
            string_repr = string_of cfg;
            nodes_pos = nodes_pos cfg;
          }]
        else []
      in cu_graph @ section_graphs @ acc
    end group []

let make_d3 ({ group; _ }: Cobol_typeck.Outputs.t) =
  let options = Options.create ~collapse_fallthru:false () in
  Cobol_unit.Collections.SET.fold
    begin fun { payload = cu; _ } acc ->
      let cfg = cfg_of ~options ~cu in
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

let make ?(d3=false) ~options (checked_doc: Cobol_typeck.Outputs.t) =
  if d3
  then make_d3 checked_doc
  else make_dot ~options checked_doc

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
