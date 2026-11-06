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

open Cobol_common.Srcloc.INFIX
open Cobol_ptree

module ANALYSIS = Superbol_project.Analysis
module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module WITNESS = Superbol_cache.Witness
module Visitor = Cobol_common.Visitor

type symbol =
  {
    name: string;
    key: string;
    kind: Lsp.Types.SymbolKind.t;
    container: string option;
    range: Lsp.Types.Range.t;
  }

type t = symbol array

(* --- Collecting --- *)

let normalize s =
  String.map (function '_' -> '-' | c -> Char.uppercase_ascii c) s

let normalize_query q =
  normalize (String.trim q)

let string_of_name_or_lit (n: name_or_literal) =
  match n with
  | Name n -> ~&n
  | _ -> Pretty.to_string "%a" pp_term n

(* Programs and other units may be nested: [units] holds the names of the
   enclosing ones, innermost first; [section] is the name of the section the
   paragraphs being visited belong to, if any. *)
type acc =
  {
    symbols: symbol list;
    units: string list;
    section: string option;
  }

let of_ptree ~filename ptree : t =
  let range_from ~loc = Lsp_position.range_of_srcloc_in ~filename loc in
  let add ~kind ~loc ?container name acc =
    let container = match container with
      | Some _ -> container
      | None -> List.nth_opt acc.units 0
    in
    { acc with
      symbols = { name; key = normalize name; kind; container;
                  range = range_from ~loc } :: acc.symbols }
  in
  let unit_folder (type a) ~kind (name: a -> string)
      ({ loc; payload }: a with_loc) acc =
    let name = name payload in
    let acc = add ~kind ~loc name acc in
    Visitor.do_children_and_then
      { acc with units = name :: acc.units; section = None }
      (fun acc' -> { acc' with units = acc.units; section = acc.section })
  in
  let data_name ~kind ~loc (name: name with_loc) acc =
    Visitor.skip_children @@ add ~kind ~loc ~&name acc
  in
  let acc =
    Cobol_ptree.Visitor.fold_compilation_group object
      inherit [acc] Cobol_ptree.Visitor.folder

      method! fold_program_unit' =
        unit_folder ~kind:Module
          (fun (u: program_unit) -> string_of_name_or_lit ~&(u.program_name))

      method! fold_function_unit' =
        unit_folder ~kind:Function
          (fun (u: function_unit) -> string_of_name_or_lit ~&(u.function_name))

      method! fold_class_definition' =
        unit_folder ~kind:Class
          (fun (c: class_definition) -> string_of_name_or_lit ~&(c.class_name))

      method! fold_interface_definition' =
        unit_folder ~kind:Interface
          (fun (i: interface_definition) ->
             string_of_name_or_lit ~&(i.interface_name))

      method! fold_method_definition' =
        unit_folder ~kind:Method
          (fun (m: method_definition) -> ~&(m.method_name))

      method! fold_paragraph' { loc; payload = p } acc =
        Visitor.skip_children @@
        match p.paragraph_name with
        | None ->
            acc
        | Some name when p.paragraph_is_section ->
            let acc = add ~kind:Namespace ~loc ~&name acc in
            { acc with section = Some ~&name }
        | Some name ->
            let container = match acc.section with
              | Some _ as s -> s
              | None -> List.nth_opt acc.units 0
            in
            add ~kind:Function ~loc ?container ~&name acc

      method! fold_data_item' { loc; payload = di } acc =
        match di.data_name with
        | Some { payload = DataName name; _ } ->
            (* condition names are nested within data items *)
            Visitor.do_children @@
            add ~kind:(if ~&(di.data_level) <= 1 || ~&(di.data_level) = 77
                       then Variable else Field) ~loc ~&name acc
        | _ ->
            Visitor.do_children acc

      method! fold_constant_item' { loc; payload = ci } =
        data_name ~kind:Constant ~loc ci.constant_name

      method! fold_rename_item' { loc; payload = ri } =
        data_name ~kind:Field ~loc ri.rename_to

      method! fold_condition_name_item' { loc; payload = c } =
        data_name ~kind:EnumMember ~loc c.condition_name

    end ptree { symbols = []; units = []; section = None }
  in
  Array.of_list (List.rev acc.symbols)

(* --- Caching --- *)

(* Symbols depend on the analysis for the LSP they are computed from, so that
   any miss on the latter entails a miss on the former. *)
let key keys =
  CACHE.shallow_key ~label:"workspace-symbols" ~inputs:(ANALYSIS.inputs keys)

let slot keys =
  ANALYSIS.slot ~label:"workspace-symbols" keys

let store cache ~keys ~label (symbols: t) =
  let key = key keys in
  match
    CACHE.store cache ~key ~label
      ~witnesses:[WITNESS.sub (ANALYSIS.key_for_lsp keys)]
      ~write_item:CACHING.write_item symbols
  with
  | Ok () ->
      (* Failing to supersede only leaves the previous version behind. *)
      ignore @@ CACHE.supersede cache ~slot:(slot keys) [key]
  | Error _ ->
      ()

let lookup cache ~keys ~resolve : t option =
  match
    CACHE.lookup cache ~key:(key keys) ~resolve ~read_item:CACHING.read_item
  with
  | Ok result -> result
  | Error _ -> None

let mem cache ~keys =
  Option.is_some (lookup cache ~keys ~resolve:(fun _ -> true))

let store_for_analysis cache ~keys ~filename ~label ?(fresh = false)
    (analysis: ANALYSIS.for_lsp) =
  match analysis with
  | { lsp_copybook = true; _ } | { lsp_checked = None; _ } ->
      ()
  | { lsp_checked = Some { ptree; _ }; _ } ->
      if fresh || not (mem cache ~keys) then
        store cache ~keys ~label (of_ptree ~filename ptree)

(* --- Searching --- *)

let is_separator = function '-' | ' ' | '.' -> true | _ -> false

(* Greedy, left-to-right subsequence matching: each character of the query is
   matched with its first occurrence in what remains of the key.  Greedy
   matching may miss a better-aligned match (e.g. at the start of a later word),
   but that only affects the ranking, and is good enough for short symbol names.
   Bonuses favor matches at word starts and consecutive matches. *)
let score ~query { key; _ } =
  let qlen = String.length query and klen = String.length key in
  if qlen = 0 then Some 0
  else if qlen > klen then None
  else
    let rec go qi ki prev_match score =
      if qi = qlen then Some score
      else if klen - ki < qlen - qi then None
      else if query.[qi] <> key.[ki] then go qi (succ ki) false score
      else
        let bonus =
          (if ki = 0 then 10
           else if is_separator key.[ki - 1] then 8
           else 0) +
          (if prev_match then 5 else 0)
        in
        go (succ qi) (succ ki) true (score + 1 + bonus)
    in
    Option.map begin fun s ->
      if String.equal query key then s + 100
      else if String.starts_with ~prefix:query key then s + 50
      else s - (klen - qlen) / 4           (* tie-break: prefer shorter names *)
    end (go 0 0 false 0)

let search ~query ~limit docs =
  let query = normalize_query query in
  let matches =
    Seq.fold_left begin fun acc (uri, symbols) ->
      Array.fold_left begin fun acc sym ->
        match score ~query sym with
        | Some s -> (s, uri, sym) :: acc
        | None -> acc
      end acc symbols
    end [] docs
  in
  let compare (s1, _, a) (s2, _, b) =
    match Int.compare s2 s1 with
    | 0 -> String.compare a.key b.key
    | c -> c
  in
  List.sort compare matches |>
  List.filteri (fun i _ -> i < limit) |>
  List.map begin fun (_, uri, { name; kind; container; range; _ }) ->
    Lsp.Types.SymbolInformation.create ()
      ~name ~kind ?containerName:container
      ~location:(Lsp.Types.Location.create ~uri ~range)
  end
