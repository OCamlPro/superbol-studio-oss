(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_cfg.Types

let cfg_options_of_json o =
  let open Yojson.Safe.Util in
  let hide_unreachable =
    Option.fold ~none:false ~some:to_bool
      (List.assoc_opt "hide_unreachable" o)
  in
  let collapse_fallthru =
    Option.fold ~none:false ~some:to_bool
      (List.assoc_opt "collapse_fallthru" o)
  in
  let in_degree_upper_limit =
    List.assoc_opt "in_degree_upper_limit" o |> Option.map to_int in
  let transformation =
    let id =
      List.assoc_opt "id" o |> Option.map to_int in
    let action = List.assoc_opt "action" o |> Option.map to_string in
    match action, id with
    | Some "descendents", Some id ->
      Some (Cobol_cfg.Options.Descendents id)
    | Some "neighborhood", Some id ->
      Some (Cobol_cfg.Options.Neighborhood id)
    | _ -> None
  in
  let hidden_nodes =
    Option.fold ~none:[] ~some:(fun o -> List.map to_int @@ to_list o)
      (List.assoc_opt "hidden_nodes" o)
  in
  let split_nodes =
    Option.fold ~none:[] ~some:(fun o -> List.map to_int @@ to_list o)
      (List.assoc_opt "split_nodes" o)
  in
  ({
    hide_unreachable;
    collapse_fallthru;
    in_degree_upper_limit;
    transformation;
    hidden_nodes;
    split_nodes;
  }: Cobol_cfg.Options.t)

let edge_to_string = function
  | FallThrough -> "f"
  | Perform -> "p"
  | Go -> "g"

let to_dot_string g =
  Pretty.to_string "%a" Cobol_cfg.Printer.pp_cfg_dot g

let to_d3_string cfg =
  let cfg_edges = CFG.fold_edges_e
      begin fun (n1, e, n2) acc ->
        Pretty.to_string "{\"source\":%d,\"target\":%d,\"type\":\"%s\"}"
          n1.id n2.id (edge_to_string e)
        ::acc
      end cfg [] in
  let cfg_nodes = CFG.fold_vertex
      begin fun n acc ->
        let name =
          match n.typ with
          | Normal (_, name)
          | Entry (`Statement name) | Entry (`Section name)
          | External name | Split name -> name
          | Collapsed _ -> Fmt.invalid_arg "Impossible to provide d3 string with \
                                           collapsed node"
          | Entry `Point -> "Entry point"
          | Entry `Paragraph -> "Entry paragraph"
        in Pretty.to_string "{\"id\":%d,\"name\":\"%s\",\"section\":\"%s\"}"
          n.id name n.section_name
           :: acc
      end cfg [] in
  let str_nodes = String.concat "," cfg_nodes in
  let str_edges = String.concat "," cfg_edges in
  Pretty.to_string "{\"links\":[%s],\"nodes\":[%s]}" str_edges str_nodes

let nodes_pos ~filename cfg =
  let assoc =
    CFG.fold_vertex begin fun n acc ->
      match n.loc with
      | None -> acc
      | Some loc ->
          let range = Lsp_position.range_of_srcloc_in ~filename loc in
          (string_of_int n.id, Lsp.Types.Range.yojson_of_t range)::acc
    end cfg []
  in
  `Assoc assoc

let doc_to_cfg_jsoono ~filename ~name ~options checked_doc =
  let cfg, cfg_with_options =
    Cobol_cfg.Builder.make ~name checked_doc
      ~options:(cfg_options_of_json options)
  in
  `Assoc [
    "string_repr_d3", `String (to_d3_string cfg);
    "string_repr_dot", `String (to_dot_string cfg_with_options);
    "nodes_pos", nodes_pos ~filename cfg;
    "name", `String name;
  ]
