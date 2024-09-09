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

type jumps =
  | Go of qualname
  | GoDepending of qualname
  | Perform of qualname

module Qualnames = Set.Make(struct
  type t = qualname
  let compare = Cobol_ptree.compare_qualname
end)

module Jumps = Set.Make(struct
    type t = jumps
    let compare j1 j2 =
      let to_int = function
        | Go _ -> 0
        | GoDepending _ -> 1
        | Perform _ -> 2 in
      match j1, j2 with
      | Go qn1, Go qn2
      | GoDepending qn1, GoDepending qn2
      | Perform qn1, Perform qn2 -> Cobol_ptree.compare_qualname qn1 qn2
      | _ -> to_int j2 - to_int j1
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
  typ: [`External | `EntryPoint | `EntryPara | `EntrySection | `Internal ];
  jumps: Jumps.t;
  will_fallthru: bool;
  terminal: bool;
}

let fullqn_to_string qn =
  Pretty.to_string "%a" Cobol_ptree.pp_qualname qn

let name_to_string (qn: qualname) =
  Cobol_ptree.(match qn with
  | Name name | Qual (name, _) -> Pretty.to_string "%a" pp_name' name)

let qn_equal qn1 qn2 = 0 == Cobol_ptree.compare_qualname qn1 qn2

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn

let full_qn' ~cu qn = full_qn ~cu ~&qn

let node_idx = ref 0

let listsplit3 l =
  List.fold_left begin fun (a_acc, b_acc, c_acc) (a, b, c) ->
    (a::a_acc, b::b_acc, c::c_acc)
  end ([], [], []) l

module JumpCollector = struct
  type acc = {
    jumps: Jumps.t;
    will_fallthru: bool;
    terminal: bool;
  }
  let init = { jumps = Jumps.empty;
               terminal = false;
               will_fallthru = true; }
  let folder ~cu = object (v)
    inherit [acc] Visitor.folder
    method! fold_goback' _ acc =
      skip @@ { acc with terminal = true; will_fallthru = false }
    method! fold_stop' _ acc =
      skip @@ { acc with terminal = true; will_fallthru = false }
    method! fold_exit' { payload = exit_stmt; _ } acc =
      skip @@
      match exit_stmt with
      | ExitSimple | ExitPerform _
      | ExitMethod _ | ExitProgram _ | ExitFunction _ -> acc
      | ExitParagraph -> { acc with will_fallthru = true }
      | ExitSection -> { acc with will_fallthru = true } (* TODO: go to next section ? *)
    method! fold_evaluate' { payload; _ } acc =
      let { eval_branches; eval_otherwise; _ }: Cobol_ptree.evaluate_stmt =
        payload in
      let jumps, terminals, unreachables = List.map begin fun branch ->
          let { jumps; terminal; will_fallthru } =
            Cobol_ptree.Visitor.fold_evaluate_branch v branch init in
          (jumps, terminal, will_fallthru)
        end eval_branches |> listsplit3 in
      let other =
        Cobol_ptree.Visitor.fold_statements v eval_otherwise init in
      skip {
        jumps = List.fold_left Jumps.union acc.jumps (other.jumps::jumps);
        will_fallthru = List.fold_left (||) other.will_fallthru unreachables;
        terminal = List.fold_left (||) other.terminal terminals;
      }
    method! fold_statement' _ ({ will_fallthru; _ } as acc) =
      if will_fallthru then do_children acc else skip acc
    method! fold_if' { payload = { then_branch; else_branch; _ }; _ } acc =
      let {  jumps; terminal; will_fallthru } =
        Cobol_ptree.Visitor.fold_statements v then_branch acc in
      let { jumps = else_jumps;
            terminal = else_terminal;
            will_fallthru = else_fallthru } =
        Cobol_ptree.Visitor.fold_statements v else_branch init in
      skip {
        jumps = Jumps.union jumps else_jumps;
        will_fallthru = will_fallthru || else_fallthru;
        terminal = terminal || else_terminal;
      }
    method! fold_goto' { payload; _ } acc =
      skip @@
      match payload with
      | GoToEntry _ -> acc (* TODO couldn't find doc *)
      | GoToSimple { target } ->
        {
          acc with
          jumps = Jumps.add (Go (full_qn' ~cu target)) acc.jumps;
          will_fallthru = false;
        }
      | GoToDepending { targets; _ } ->
        Cobol_common.Basics.NEL.(
          targets
          |> map ~f:(full_qn' ~cu)
          |> fold_left ~f:begin fun acc target ->
            Jumps.add (GoDepending target) acc
          end acc.jumps)
        |> begin fun jumps -> { acc with jumps } end
    method! fold_perform_target' { payload; _ } acc =
      let start = full_qn' ~cu payload.perform_target.procedure_start in
      skip { acc with jumps = Jumps.add (Perform start) acc.jumps }
  end
end

let build_node ~default_name ~cu paragraph =
  let open JumpCollector in
  let { jumps; will_fallthru; terminal; } =
    Visitor.fold_procedure_paragraph' (folder ~cu) paragraph init in
  node_idx:=!node_idx+1;
  let qid, loc = match ~&paragraph.paragraph_name with
    | None -> default_name, ~@paragraph
    | Some qn -> full_qn' ~cu qn, ~@qn in
  let name = fullqn_to_string qid
  in {
    id = !node_idx;
    qid;
    names = NEL.One name;
    loc = Some loc;
    jumps;
    will_fallthru;
    terminal;
    typ = `Internal;
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
  | Perform
  | Go

module Edge = struct
   type t = edge
   let compare = Stdlib.compare
   let default = FallThrough
   let to_string = function
     | FallThrough -> "f"
     | Perform -> "c"
     | Go -> "u"
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

let vertex_name_record { names; _ } =
  Pretty.to_string "%a"
    (NEL.pp ~fopen:"{" ~fclose:"}" ~fsep:"|" Fmt.string)
    (NEL.rev names)

(* Graph.Graphviz.DotAttributes *)
module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes (_,s,_) =
      [`Style (match s with
           | FallThrough -> `Dotted
           | Perform -> `Dashed
           | Go -> `Solid)]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes ({ typ; _ } as n) =
      let label, shape =
        match typ with
        | `EntryPara -> "Entry\nparagraph", [`Shape `Doubleoctagon]
        | `EntryPoint -> "Entry\npoint", [`Shape `Doubleoctagon]
        | `EntrySection -> NEL.hd n.names, [`Shape `Doubleoctagon]
        | `External -> NEL.hd n.names, [`Shape `Plaintext]
        | `Internal -> vertex_name_record n, []
      in `Label label :: shape
    let default_vertex_attributes _ = [`Shape `Record]
    let graph_attributes _ = []
    let vertex_name { id; _ } = string_of_int id
  end)

let to_dot_string g =
  Pretty.to_string "%a" Dot.fprint_graph g

let dummy_node ?(typ=`External) (qn: qualname) =
  let loc = match qn with
  | Cobol_ptree.Name name -> ~@name
  | Qual (name, _) -> ~@name in
  node_idx:= !node_idx + 1;
  {
    id = !node_idx;
    qid = qn;
    loc = Some loc;
    names = NEL.One (fullqn_to_string qn);
    jumps = Jumps.empty;
    will_fallthru = true;
    terminal = false;
    typ;
  }

let clone_node node =
  node_idx:= !node_idx + 1;
  { node with id = !node_idx; }

let qmap_find_or_add qmap qn =
  match Qmap.find_opt qn qmap with
  | None -> let node = dummy_node qn in
    Qmap.add qn node qmap, node
  | Some node -> qmap, node

let rec build_edges ~vertexes g nodes =
  let g, vertexes = match nodes with
    | ({ jumps; _ } as current)::_ ->
      Jumps.fold begin fun uncond (g, vertexes) ->
        match uncond with
        | GoDepending jump_to
        | Go jump_to ->
          let vertexes, next = qmap_find_or_add vertexes jump_to in
          Cfg.add_edge_e g (current, Go, next),
          vertexes
        | Perform jump_to ->
          let vertexes, next = qmap_find_or_add vertexes jump_to in
          Cfg.add_edge_e g (current, Perform, next),
          vertexes
      end jumps (g, vertexes)
    | [] -> g, vertexes
  in
  match nodes with
  | ({ will_fallthru; _ } as current)::next::tl
    when will_fallthru ->
    build_edges ~vertexes (Cfg.add_edge g current next) (next::tl)
  | _::tl -> build_edges ~vertexes g tl
  | [] -> g


let do_collapse_fallthru g =
  Cfg.fold_vertex begin fun n cfg ->
    match Cfg.pred_e cfg n with
    | [(({ typ = `Internal; _ } as pred), FallThrough, _)] ->
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
        if Cfg.in_degree cfg n <= 0 && n.typ == `Internal
        then true, Cfg.remove_vertex cfg n
        else did_remove, cfg
      end cfg (false, cfg)
    in
    if did_remove then aux cfg else cfg
  in aux g

let do_shatter_hubs ?(limit=20) g =
  Cfg.fold_vertex begin fun n cfg ->
    if Cfg.in_degree cfg n >= limit
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

let cfg_of_nodes nodes =
  let g, vertexes = List.fold_left begin fun (g, vertexes) node ->
      Cfg.add_vertex g node,
      Qmap.add node.qid node vertexes
    end (Cfg.empty, Qmap.empty) nodes
  in
  build_edges ~vertexes g nodes

let handle_cfg_options ~(options: Cfg_options.t) cfg =
  cfg
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
      { hd with id=0; typ = `EntryPara; names = NEL.One "Entry paragraph" }::tl
    | l ->
      { (dummy_node ~typ:`EntryPoint default_name)
        with id=0; names = NEL.One "Entry point" } :: l
  end
  |> cfg_of_nodes

let cfg_of_section ~cu ({ section_paragraphs; section_name }: procedure_section) =
  node_idx := 0;
  let default_name = ~&section_name in
  let nodes =
    List.fold_left begin fun acc p ->
      let node = build_node ~default_name ~cu p  in
      let name = name_to_string node.qid in
      { node with names = NEL.One name } :: acc
    end [] section_paragraphs.list
    |> List.rev in
  let nodes = match nodes with
    | entry::tl -> { entry with typ = `EntrySection }::tl
    | [] -> []
  in cfg_of_nodes nodes

type graph = {
  name: string;
  string_repr_dot: string;
  string_repr_d3: string;
  nodes_pos: (int * srcloc) list
}

let nodes_pos cfg =
  Cfg.fold_vertex begin fun n acc ->
    match n.loc with
    | None -> acc
    | Some loc -> (n.id, loc)::acc
  end cfg []

let to_d3_string cfg =
  let cfg_edges = Cfg.fold_edges_e
      begin fun (n1, e, n2) acc ->
        Pretty.to_string "{\"source\":%d,\"target\":%d,\"type\":\"%s\"}"
          n1.id n2.id (Edge.to_string e)
        ::acc
      end cfg [] in
  let cfg_nodes = Cfg.fold_vertex
      begin fun n acc ->
        Pretty.to_string "{\"id\":%d,\"name\":\"%s\"}"
          n.id (fullqn_to_string n.qid)
        :: acc
      end cfg [] in
  let str_nodes = String.concat "," cfg_nodes in
  let str_edges = String.concat "," cfg_edges in
  Pretty.to_string "{\"links\":[%s],\"nodes\":[%s]}" str_edges str_nodes

let make_cfg ?(graph_name=None) ({ group; _ }: Cobol_typeck.Outputs.t) =
  let is_to_include : string -> bool =
    match graph_name with
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
            else Some ( name, cfg_of_section ~cu ~&sec)
        end cu.unit_procedure.list in
      let cu_graph =
        if is_to_include ((~&) cu.unit_name)
        then [((~&)cu.unit_name, cfg_of ~cu)]
        else []
      in cu_graph @ section_graphs @ acc
    end group []

let make ~(options: Cfg_options.t) (checked_doc: Cobol_typeck.Outputs.t) =
  make_cfg ~graph_name:options.graph_name checked_doc
  |> List.map begin fun (name, cfg) ->
    let cfg_with_options = handle_cfg_options ~options cfg in
    {
      name;
      string_repr_dot = to_dot_string cfg_with_options;
      string_repr_d3 = to_d3_string cfg;
      nodes_pos = nodes_pos cfg;
    }
  end
