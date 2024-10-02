(* what licence here ? *)

open Cobol_cfg.Builder

let create_cfg_options o =
  let open Yojson.Safe.Util in
  let hide_unreachable =
    try
      List.assoc "hide_unreachable" o |> to_bool
    with Not_found -> false
  in
  let collapse_fallthru =
    try
      List.assoc "collapse_fallthru" o |> to_bool
    with Not_found -> false
  in
  let shatter_hubs =
    List.assoc_opt "shatter_hubs" o |> Option.map to_int in
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
    try
      List.assoc "hidden_nodes" o |> to_list |> (List.map to_int)
    with Not_found -> []
  in
  let split_nodes =
    try
      List.assoc "split_nodes" o |> to_list |> (List.map to_int)
    with Not_found -> []
  in
  ({
    hide_unreachable;
    collapse_fallthru;
    shatter_hubs;
    transformation;
    hidden_nodes;
    split_nodes;
  }: Cobol_cfg.Options.t)

let vertex_name_record names =
  Pretty.to_string "%a"
    (Cobol_common.Basics.NEL.pp ~fopen:"{" ~fclose:"}" ~fsep:"|" Fmt.string)
    names

module Dot = Graph.Graphviz.Dot(struct
    include Cobol_cfg.Builder.Cfg
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

let edge_to_string = function
  | FallThrough -> "f"
  | Perform -> "p"
  | Go -> "g"

let to_dot_string g =
  Pretty.to_string "%a" Dot.fprint_graph g

let to_d3_string cfg =
  let cfg_edges = Cfg.fold_edges_e
      begin fun (n1, e, n2) acc ->
        Pretty.to_string "{\"source\":%d,\"target\":%d,\"type\":\"%s\"}"
          n1.id n2.id (edge_to_string e)
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

let nodes_pos ~filename cfg =
  let assoc = Cfg.fold_vertex begin fun n acc ->
    match n.loc with
    | None -> acc
    | Some loc ->
        let range = Lsp_position.range_of_srcloc_in ~filename loc in
        (string_of_int n.id, Lsp.Types.Range.yojson_of_t range)::acc
  end cfg []
  in `Assoc assoc

let doc_to_cfg_jsoono ~filename ~name ~options checked_doc =
  let options = create_cfg_options options in
  let (cfg, cfg_with_options) =
    make ~options ~name checked_doc in
  `Assoc [
    ("string_repr_d3", `String (to_d3_string cfg));
    ("string_repr_dot", `String (to_dot_string cfg_with_options));
    ("nodes_pos", nodes_pos ~filename cfg);
    ("name", `String name);]
