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
open Cfg_jumps
module NEL = Cobol_common.Basics.NEL

(* TYPES AND HELPERS *)

type qualname = Cobol_ptree.qualname

type display_name_type =
  | Full
  | Short

let qn_to_strings : qualname -> string * string = function
  | Name { payload; _ } -> payload, ""
  | Qual({ payload = para; _ }, Name { payload = sec; _ }) -> para, sec
  | _ -> raise @@ Invalid_argument
      "qn with more than 2 qualification levels cannot \
       come from a paragraph or section"

let qn_to_fullname qn =
  let name, qual = qn_to_strings qn in
  if qual == ""
  then name
  else name ^ " IN " ^ qual

let incr ref =
  ref := !ref + 1;
  !ref

let prefix_to_string prefix =
  begin match prefix with
    | Cobol_ptree.CallGeneral i ->
      Pretty.to_string "CALL %a" Cobol_ptree.pp_ident_or_strlit i
    | CallProto { prototype = CallProtoNested; _ }-> "CALL NESTED"
    | CallProto { called; prototype = CallProtoIdent i }->
      Pretty.to_string "CALL %a AS %a"
        Fmt.(option Cobol_ptree.pp_ident_or_strlit) called
        Cobol_ptree.pp_ident i
  end |> Str.global_replace (Str.regexp "\"") "\\\""

let entry_stmt_to_string_loc = function
  | Cobol_ptree.EntryForGoTo { payload; loc }
  | Cobol_ptree.EntryUsing { entry_name={ payload; loc }; _ }
  | Cobol_ptree.EntrySimple { payload; loc } ->
    Str.global_replace (Str.regexp "\"") "\\\""
      (Pretty.to_string "ENTRY %a" Cobol_ptree.pp_alphanum payload),
    loc

(* CFG MODULE *)

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

let is_entry n =
  match n.typ with
  | External _ | Normal _ | Collapsed _ | Split _ -> false
  | Entry _ -> true

type edge =
  | FallThrough
  | Perform
  | Go

  module Node = struct
    type t = node
  let compare node other =
    Int.compare node.id other.id
  let hash node =
    Hashtbl.hash node.id
  let equal node other =
    Int.equal node.id other.id
end

module Edge = struct
   type t = edge
   let compare = Stdlib.compare
   let default = FallThrough
   let to_string = function
     | FallThrough -> "f"
     | Perform -> "p"
     | Go -> "g"
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

(* DEFAULT CFG BUILDER FUNCTION *)

let node_idx = ref 0
let call_stmt_section_name = "__CALL_STMT__"

let reset_global_counter () =
  node_idx := 0

let build_node ?(is_section=false) ?(display_name_type=Full) ~cu paragraph =
  let { jumps; will_fallthru; terminal; skip_remaining = _ }
    : JumpsCollector.acc = Visitor.fold_procedure_paragraph'
      (JumpsCollector.folder ~cu) paragraph JumpsCollector.init in
  let typ, loc, section_name = match ~&paragraph.paragraph_name with
    | None -> Entry `Paragraph, ~@paragraph, ""
    | Some qn ->
      let fullqn = full_qn' ~cu qn in
      let full_name = qn_to_fullname fullqn in
      let short_name, section_name =
        let name, qualifier = qn_to_strings fullqn in
        name, if is_section then name else qualifier
      in
      let display_name = match display_name_type with
        | Full -> full_name
        | Short -> short_name
      in Normal (full_name, display_name), ~@qn, section_name
  in {
    id = incr node_idx;
    section_name;
    loc = Some loc;
    jumps;
    will_fallthru;
    terminal;
    typ;
  }

let new_node ~typ =
  let loc_of ~(qn: qualname) = match qn with
    | Cobol_ptree.Name name
    | Qual (name, _) -> ~@name in
  let typ, loc, section_name = match typ with
    | `External qn ->
      let _para, section = qn_to_strings qn in
      External (qn_to_fullname qn), Some (loc_of ~qn), section
    | `EntryPoint -> Entry `Point, None, ""
    | `EntryStmt ({ payload; loc }, id) ->
      Entry (`Statement payload), Some loc, id
    | `Call s ->
      External s, None, call_stmt_section_name
  in {
    id = incr node_idx;
    section_name;
    loc;
    jumps = Jumps.empty;
    will_fallthru = true;
    terminal = false;
    typ;
  }

let build_edges nodes =
  let module StringMap = Map.Make(String) in
  let find_or_add smap ~typ =
    let string_id = match typ with
      | `External qn -> qn_to_fullname qn
      | `EntryStmt ({ payload; _ }, _) -> payload
      | `Call name -> name in
    match StringMap.find_opt string_id smap with
    | Some node -> smap, node
    | None ->
      let node = new_node ~typ in
      StringMap.add string_id node smap, node
  in
  let rec edge_builder_aux ~vertexes g nodes =
    let g, vertexes = match nodes with
      | ({ jumps; _ } as current)::_ ->
        Jumps.fold begin fun uncond (g, vertexes) ->
          match uncond with
          | GoDepending qn
          | Go qn ->
            let vertexes, next = find_or_add vertexes ~typ:(`External qn) in
            Cfg.add_edge_e g (current, Go, next),
            vertexes
          | Perform qn ->
            let vertexes, next = find_or_add vertexes ~typ:(`External qn) in
            Cfg.add_edge_e g (current, Perform, next),
            vertexes
          | Call prefix ->
            let vertexes, next =
              find_or_add vertexes ~typ:(`Call (prefix_to_string prefix)) in
            Cfg.add_edge_e g (current, Perform, next),
            vertexes
          | Entry entry_stmt ->
            let name, loc = entry_stmt_to_string_loc entry_stmt in
            let vertexes, next =
              find_or_add vertexes ~typ:(`EntryStmt (name &@ loc, current.section_name)) in
            Cfg.add_edge_e g (next, FallThrough, current),
            vertexes
        end jumps (g, vertexes)
      | [] -> g, vertexes
    in
    match nodes with
    | ({ will_fallthru; _ } as current)::next::tl
      when will_fallthru ->
      edge_builder_aux ~vertexes (Cfg.add_edge g current next) (next::tl)
    | _::tl -> edge_builder_aux ~vertexes g tl
    | [] -> g
  in
  let g, vertexes = List.fold_left begin fun (g, vertexes) node ->
      Cfg.add_vertex g node,
      match node.typ with
      | Normal (full_name, _) -> StringMap.add full_name node vertexes
      | _ ->  vertexes
    end (Cfg.empty, StringMap.empty) nodes
  in
  edge_builder_aux ~vertexes g nodes

let cfg_of ~(cu: cobol_unit) =
  reset_global_counter ();
  let nodes = List.fold_left begin fun acc block ->
      match block with
      | Paragraph para ->
        build_node ~cu para :: acc
      | Section { payload = { section_paragraphs; _ }; _ } ->
        fst @@ List.fold_left begin fun (acc, is_section) p ->
            build_node ~is_section ~cu p :: acc,
            false
          end (acc, true) section_paragraphs.list
    end [] cu.unit_procedure.list
  in
  List.rev nodes
  |> begin function (* adding entry point if not already present *)
    | ({ typ = Entry _; _ } as hd )::tl -> { hd with id=0 }::tl
    | l -> { (new_node ~typ:`EntryPoint) with id=0 } :: l
  end
  |> build_edges

let cfg_of_section ~cu ({ section_paragraphs; _ }: procedure_section) =
  reset_global_counter ();
  let nodes =
    List.fold_left begin fun (acc, is_section) p ->
      build_node ~is_section ~display_name_type:Short ~cu p :: acc,
      false
    end ([], true) section_paragraphs.list
    |> fst
    |> List.rev in
  begin match nodes with
    | ({ typ = Normal (_, name); _ } as entry)::tl ->
      { entry with typ = Entry (`Section name) }::tl
    | l -> l end
  |> build_edges

let cfgs_of_doc ?(graph_name=None) ({ group; _ }: Cobol_typeck.Outputs.t) =
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
            if is_to_include name
            then Some (name, cfg_of_section ~cu ~&sec)
            else None
        end cu.unit_procedure.list in
      let cu_graph =
        if is_to_include ((~&) cu.unit_name)
        then [((~&)cu.unit_name, cfg_of ~cu)]
        else []
      in cu_graph @ section_graphs @ acc
    end group []

(* CFG OPTIONS HANDLER *)

let do_collapse_fallthru g =
  let module IdMap = Map.Make(Int) in
  let get_names_if_collapsable { typ; _ } =
    match typ with
    | Collapsed names -> Some names
    | Normal (_, name) -> Some (NEL.One name)
    | Entry _ | External _ | Split _ -> None in
  let collapse_node ~cfg ~id_map ~node ~pred n_names pred_names =
    let cfg = Cfg.fold_succ_e begin fun (_, e, next) cfg ->
        Cfg.add_edge_e cfg (pred, e, next)
      end cfg node cfg in
    let id_map = IdMap.update pred.id
        begin function
          | None -> Some NEL.(n_names @ pred_names)
          | Some names -> Some NEL.(n_names @ names)
        end id_map in
    Cfg.remove_vertex cfg node, id_map
  in
  let id_map = IdMap.empty in
  let cfg, id_map =
    Cfg.fold_vertex begin fun node (cfg, id_map) ->
      match get_names_if_collapsable node with
      | None -> (cfg, id_map)
      | Some n_names ->
        match Cfg.pred_e cfg node with
        | [(({ typ = Normal (_, pred_name); _ } as pred), FallThrough, _)] ->
          collapse_node ~cfg ~id_map ~node ~pred n_names (NEL.One pred_name)
        | [(({ typ = Collapsed pred_names ; _ } as pred), FallThrough, _)] ->
          collapse_node ~cfg ~id_map ~node ~pred n_names pred_names
        | _ -> cfg, id_map
    end g (g, id_map) in
  Cfg.map_vertex begin fun node ->
    match IdMap.find_opt node.id id_map with
    | None -> node
    | Some names -> { node with typ = Collapsed (NEL.rev names) }
  end cfg

let do_hide_unreachable ~except g =
  let rec aux cfg =
    let did_remove, cfg =
      Cfg.fold_vertex begin fun n (did_remove, cfg) ->
        if Cfg.in_degree cfg n <= 0 && not (is_entry n)
        && not (List.mem n.id except)
        then true, Cfg.remove_vertex cfg n
        else did_remove, cfg
      end cfg (false, cfg)
    in
    if did_remove then aux cfg else cfg
  in aux g

let clone_node node =
  { node with id = incr node_idx; }

let do_shatter_nodes ~ids ~limit g =
  let shatter_typ { typ; _ } =
    match typ with
    | External name -> Some (External name, true)
    | Normal (_, name) -> Some (Split name, false)
    | Entry _ | Split _ | Collapsed _ -> None
  in
  let is_above_limit n =
    match limit with
    | Some limit -> Cfg.in_degree g n >= limit
    | None -> false
  in
  Cfg.fold_vertex begin fun n cfg ->
    match shatter_typ n with
    | Some (typ, remove_original)
      when is_above_limit n || List.mem n.id ids ->
        let cfg = Cfg.fold_pred_e begin fun edge cfg ->
        let cfg = Cfg.remove_edge_e cfg edge in
        let n_clone = { (clone_node n) with typ } in
        let (pred, edge, _) = edge in
        let cfg = Cfg.add_edge_e cfg (pred, edge, n_clone) in
        cfg
      end cfg n cfg in
      if remove_original
      then Cfg.remove_vertex cfg n
      else cfg
    | _ -> cfg
  end g g

let find_node_with ~id cfg =
  Cfg.fold_vertex begin fun node -> function
    | None when node.id == id -> Some node
    | acc -> acc
  end cfg None

let restrict_to_descendents id cfg =
  let module Ids = Set.Make(Int) in
  match find_node_with ~id cfg with
  | None -> cfg
  | Some node ->
    let ids = Ids.singleton node.id in
    let module Dfs = Graph.Traverse.Dfs(Cfg) in
    let ids = Dfs.fold_component begin fun node ids ->
        Ids.add node.id ids
      end ids cfg node in
    Cfg.fold_vertex begin fun node cfg ->
      if Ids.mem node.id ids
      then cfg
      else Cfg.remove_vertex cfg node
    end cfg cfg


let max_depth = 3
let restrict_to_neighborhood id cfg =
  let module Nodes = Set.Make(Node) in
  match find_node_with ~id cfg with
  | None -> cfg
  | Some node ->
    let nodes = Nodes.singleton node in
    let rec explore prev_depth_nodes explored_nodes depth =
      if depth > max_depth
      then Nodes.empty, explored_nodes
      else
        let next_depth_nodes = Nodes.fold begin fun node new_nodes ->
          Cfg.fold_succ begin fun succ new_nodes ->
            if Nodes.mem succ explored_nodes
            then new_nodes
            else Nodes.add succ new_nodes
          end cfg node new_nodes
        end prev_depth_nodes Nodes.empty in
        let explored_nodes = Nodes.union explored_nodes prev_depth_nodes in
        explore next_depth_nodes explored_nodes (depth+1)
    in
    let _, reachables = explore nodes nodes 0 in
    Cfg.fold_vertex begin fun node cfg ->
      if Nodes.mem node reachables
      then cfg
      else Cfg.remove_vertex cfg node
    end cfg cfg

let remove_nodes ids cfg =
  List.fold_left begin fun cfg id ->
  match find_node_with ~id cfg with
  | None -> cfg
  | Some node -> Cfg.remove_vertex cfg node
  end cfg ids

let handle_cfg_options ~(options: Cfg_options.t) cfg =
  let unreachable_expections =
    match options.transformation with
    | Some Cfg_options.Neighborhood id
    | Some Cfg_options.Descendents id -> [id]
    | None -> [] in
  cfg
  |> (match options.transformation with
      | Some Cfg_options.Descendents id -> restrict_to_descendents id
      | Some Cfg_options.Neighborhood id -> restrict_to_neighborhood id
      | _ -> Fun.id)
  |> (if options.hide_unreachable
      then do_hide_unreachable ~except:unreachable_expections else Fun.id)
  |> (match options.hidden_nodes with
      | [] -> Fun.id
      | l -> remove_nodes l)
  |> (if options.collapse_fallthru then do_collapse_fallthru else Fun.id)
  (* IMPORTANT: shatter needs to be after collapse, or else it's possible
     to find a collapsed node linked to duplicate shattered nodes *)
  |> do_shatter_nodes ~ids:options.split_nodes ~limit:options.shatter_hubs

(* CFG TO STRING FORMATTERS *)

let vertex_name_record names =
  Pretty.to_string "%a"
    (NEL.pp ~fopen:"{" ~fclose:"}" ~fsep:"|" Fmt.string)
    names

module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes (_,s,_) =
      [`Style (match s with
           | FallThrough -> `Dotted
           | Perform -> `Dashed
           | Go -> `Solid)]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes { typ; _ } =
      let label, attributes =
        match typ with
        | Entry (`Section name) -> name, [`Shape `Doubleoctagon]
        | Entry (`Statement name) -> name, [`Shape `Doubleoctagon]
        | Entry `Point -> "Entry\npoint", [`Shape `Doubleoctagon]
        | Entry `Paragraph -> "Entry\nparagraph", [`Shape `Doubleoctagon]
        | External name -> name, [`Shape `Plaintext]
        | Split name -> name, [`Style `Dashed]
        | Normal (_, name) -> name, []
        | Collapsed names -> vertex_name_record names, [`Shape `Record]
      in `Label label :: attributes
    let default_vertex_attributes _ = [`Shape `Box]
    let graph_attributes _ = []
    let vertex_name { id; _ } = string_of_int id
  end)

let to_dot_string g =
  Pretty.to_string "%a" Dot.fprint_graph g

let to_d3_string cfg =
  let cfg_edges = Cfg.fold_edges_e
      begin fun (n1, e, n2) acc ->
        Pretty.to_string "{\"source\":%d,\"target\":%d,\"type\":\"%s\"}"
          n1.id n2.id (Edge.to_string e)
        ::acc
      end cfg [] in
  let cfg_nodes = Cfg.fold_vertex
      begin fun n acc ->
        let name =
          match n.typ with
          | Normal (_, name)
          | Entry (`Statement name) | Entry (`Section name)
          | External name | Split name -> name
          | Collapsed _ ->
            raise @@ Invalid_argument
              "Impossible to provide d3 string with collapsed node"
          | Entry `Point -> "Entry point"
          | Entry `Paragraph -> "Entry paragraph"
        in Pretty.to_string "{\"id\":%d,\"name\":\"%s\",\"section\":\"%s\"}"
          n.id name n.section_name
           :: acc
      end cfg [] in
  let str_nodes = String.concat "," cfg_nodes in
  let str_edges = String.concat "," cfg_edges in
  Pretty.to_string "{\"links\":[%s],\"nodes\":[%s]}" str_edges str_nodes

(* GRAPH OUTPUT FORMAT *)

let nodes_pos cfg =
  Cfg.fold_vertex begin fun n acc ->
    match n.loc with
    | None -> acc
    | Some loc -> (n.id, loc)::acc
  end cfg []

type graph = {
  name: string;
  string_repr_dot: string;
  string_repr_d3: string;
  nodes_pos: (int * srcloc) list
}

let make ~(options: Cfg_options.t) (checked_doc: Cobol_typeck.Outputs.t) =
  cfgs_of_doc ~graph_name:options.graph_name checked_doc
  |> List.map begin fun (name, cfg) ->
    let cfg_with_options = handle_cfg_options ~options cfg in
    {
      name;
      string_repr_dot = to_dot_string cfg_with_options;
      string_repr_d3 = to_d3_string cfg;
      nodes_pos = nodes_pos cfg;
    }
  end
