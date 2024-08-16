(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

(* representation of a node -- must be hashable *)
module Node = struct
   type t = Cobol_ptree.qualname
   let compare = Cobol_ptree.compare_qualname
   let hash = Hashtbl.hash
   let equal a b = Cobol_ptree.compare_qualname a b == 0
end

(* (* representation of an edge -- must be comparable *) *)
(* module Edge = struct *)
(*    type t = string *)
(*    let compare = Stdlib.compare *)
(*    let equal = (=) *)
(*    let default = "" *)
(* end *)

(* a functional/persistent graph *)

module Cfg =  Graph.Persistent.Digraph.Concrete(Node)

module Dot = Graph.Graphviz.Dot(struct
    include Cfg
    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = []
    let default_vertex_attributes _ = [`Shape `Box]
    let graph_attributes _ = []
    let vertex_name qn =
      Pretty.to_string "\"%a\""Cobol_ptree.pp_qualname qn
      |> Str.global_replace (Str.regexp "\n") " "

  end)

open Cobol_unit
open Cobol_common.Srcloc.INFIX
open Cobol_common.Srcloc.TYPES
open Cobol_unit.Types
open Cobol_common.Visitor

type 'a node = {
  name: 'a;
  has_unconditial_jump: bool;
  jumps_to: 'a list;
}

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn


let build_jump_list ~cu paragraph =
  Visitor.fold_procedure_paragraph'
    object
      inherit [_] Visitor.folder
      method! fold_goback' _ (_, jumps) = skip (true, jumps)
      method! fold_goto' { payload; _ } (unconditiona_jump, jumps) =
        skip @@
        match payload with
        | GoToEntry _ -> (unconditiona_jump, jumps) (* TODO couldn't find doc *)
        | GoToSimple { targets; depending_on } ->
          (
            unconditiona_jump || Option.is_none depending_on,
            (Cobol_common.Basics.NEL.to_list targets
            |> List.map (~&)
            |> List.map (full_qn ~cu))
            @ jumps
          )

    end
    paragraph (false, [])

let rec build_edges g = function
  | { name = current; jumps_to; has_unconditial_jump }::next::tl ->
    let g = List.fold_left (Fun.flip Cfg.add_edge current) g jumps_to in
    if has_unconditial_jump
    then build_edges g (next::tl)
    else build_edges (Cfg.add_edge g current next.name) (next::tl)
  | [{ name = current; jumps_to; has_unconditial_jump = _ }] ->
      List.fold_left (Fun.flip Cfg.add_edge current) g jumps_to
  | [] -> g

let cfg_of ~(cu: cobol_unit) =
  let graph_name = Cobol_ptree.Name cu.unit_name in
  let nodes = List.fold_left begin fun acc block ->
      match block with
      | Paragraph para ->
        let name =
          match block_name block with
          | None -> graph_name
          | Some { payload = qn; _ } -> full_qn ~cu qn
        in
        let has_unconditial_jump, jumps_to = build_jump_list ~cu para in
        let node = { name; has_unconditial_jump ; jumps_to }
        in [node] :: acc
      | Section { payload = { section_paragraphs; _ }; _ } ->
        Fun.flip List.cons acc @@
        List.filter_map (fun p ->
            match block_name @@ Paragraph p with
            | None -> None
            | Some { payload = qn; _ } ->
              let has_unconditial_jump, jumps_to = build_jump_list ~cu p in
              Some { name = full_qn ~cu qn; has_unconditial_jump; jumps_to })
          section_paragraphs.list
    end [] cu.unit_procedure.list
              |> List.rev |> List.flatten
  in
  let g = List.fold_left begin fun g node ->
      Cfg.add_vertex g node.name
    end Cfg.empty nodes
  in build_edges g nodes

let string_of g =
  Pretty.to_string "%a" Dot.fprint_graph g

let make (checked_doc: Cobol_typeck.Outputs.t) =
  let graphs = Cobol_unit.Collections.SET.fold
      begin fun { payload = cu; _ } acc ->
        acc ^ "\n" ^ (string_of @@ cfg_of ~cu)
      end checked_doc.group ""
  in
  graphs

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
