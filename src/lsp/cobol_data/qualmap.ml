(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_ast
open Cobol_common.Srcloc.INFIX

module QUAL_NAME = struct
  type t = qualname [@@deriving show]

  let compare = Cobol_ast.compare_qualname

end

module QualNameMap = struct
  include Map.Make(QUAL_NAME)
  let pp ?(sep=(fun fmt -> Fmt.pf fmt "@;")) pp_elt fmt m =
    iter
      (fun key elt ->
        Fmt.pf fmt "%a: %a%t" pp_qualname key pp_elt elt sep)
      m
end

module QualNameSet = struct
  include Set.Make(QUAL_NAME)
  let pp ?(sep=(fun fmt -> Fmt.pf fmt "@ ")) fmt s =
    iter
      (fun elt ->
         Fmt.pf fmt "%a%t" pp_qualname elt sep)
      s
end

module NameSet = struct
  include Set.Make(struct
    open Cobol_common.Srcloc.TYPES
    type t = name with_loc
    let compare {payload = p1; _} {payload = p2; _} =
      compare p1 p2
  end)
  let pp ppf s = Fmt.(braces (seq pp_name')) ppf (to_seq s) [@@warning "-32"] (*unused, for debug mainly*)
end

type 'a binding = {
  value: 'a;
  full_key: QUAL_NAME.t;
  simple_name: name;
  contained_in: name with_loc list; (*Invariant: The first element is the 01 name, with the following
                                      ones in increasing level numbers *)
} [@@deriving show]

type 'a t = {
  base_keys: QualNameSet.t;
  bindings: 'a binding QualNameMap.t;
} [@@deriving show]

let empty = { base_keys = QualNameSet.empty;
              bindings = QualNameMap.empty; }

(** [take_after pred list] returns the list with all the elements that are after
    the first element that verifies [pred element] or the empty list if no
    element verifies [pred]. *)
let rec take_after pred = function
  | hd::tl when pred hd -> tl
  | _::tl -> take_after pred tl
  | [] -> []

(** [get_lower_key qualname] returns {!qualname} without the last [Name _] in
    it, or [Name name] when qualname is [Qual (name, Name _) | Name name] *)
let rec get_lower_key qualname =
  match qualname with
  | Qual (n, Name _) ->
      (Name n: qualname)
  | Qual (n, qn) ->
      let rest = get_lower_key qn in
      Qual (n, rest)
  | _ -> qualname

(** [find_with_subkey key map] returns the map with all the qualnames that can be accessed
    with the key [key]. *)
let rec find_with_subkey key map =
  match key with
  | Qual (_, Qual _) ->
      (*we first look for all the items that are contained in the last qualifier. *)
      let highest_level_name = Cobol_ast.major_qualifier_of_qualname key in
      let new_map =
        QualNameMap.filter_map
          (fun _ ({contained_in; _} as binding) ->
             let rest =
               take_after (fun {payload; _} ->
                   payload = ~&highest_level_name) contained_in
             in
             if rest <> [] then
                 Some ({binding with contained_in = rest})
             else
               None)
          map
      in
      (* We keep looking for an item without the last qualifier *)
      find_with_subkey (get_lower_key key) new_map
  | Qual (_, Name n ) ->
      let new_map =
        QualNameMap.filter
          (fun _ {contained_in; _ } ->
             List.exists (fun {payload; _} -> payload = ~&n) contained_in)
          map
      in
      (* We keep looking for an item without the last qualifier *)
      find_with_subkey (get_lower_key key) new_map
  | Name name ->
      QualNameMap.filter
        (fun _ ({simple_name; _}) -> simple_name = ~&name)
        map

let find_binding key map =
  if QualNameSet.mem key map.base_keys then
    (QualNameMap.find key map.bindings)
  else
    let simple_name' = Cobol_ast.qualifier_of_qualname key in
    (* We look for all the qualnames that have the simple name `simple_name'`. *)
    let new_map =
      QualNameMap.filter (fun _ {simple_name; _} -> simple_name = ~&simple_name') map.bindings
    in
    (* If we find several then we look for names with the subkey `key`. *)
    let new_map = find_with_subkey key new_map in
    if QualNameMap.cardinal new_map = 1 then
      (snd @@ QualNameMap.choose new_map)
    else
      raise Not_found

(** [find qualname map] returns the unique element [e] of [map] that can be qualified with
    [qualname]. Raises [Not_found] if the element is not unique, or does not exists in [map]. *)
let find key map =
  (find_binding key map).value

(** [find_opt qualname map] returns [Some elt] if [elt] is the unique element of [map] that
    can be qualified by [qualname]. It returns [None] if there is no element or the uniqueness
    is not respected. *)
let find_opt key map =
  try Some (find key map)
  with Not_found -> None

(** [find_all qualname map] returns all the elements that can be qualified with [qualname] in [map]. *)
let find_all key map =
  let simple_name' = Cobol_ast.qualifier_of_qualname key in
  let new_map =
    QualNameMap.filter (fun _ {simple_name; _} -> simple_name = ~&simple_name') map.bindings
    |> find_with_subkey key
  in
  let binding_list = List.of_seq @@ QualNameMap.to_seq new_map in
  List.map (fun (_, elt) -> (elt.full_key, elt.value)) binding_list

(** [find_full_qualname qualname map] returns the full name of [qualname] if it is unique *)
let find_full_qualname key map =
  (find_binding key map).full_key

(** [find_full_qualname_opt qualname map] returns [Some full_name] with [full_name] being the
    fully qualified name of [qualname] if it exists, [None] otherwise*)
let find_full_qualname_opt key map =
  try Some ((find_binding key map).full_key)
  with _ -> None

let make_contained_in key =
  let rec aux acc key =
    match key with
    | Qual (name, qn) ->
        aux (name::acc) qn
    | Name name ->
        name::acc
  in
  match (key: qualname) with
  | Qual (_, qn) -> aux [] qn
  | Name _ -> []

(** [add qualname value map] returns [map] with [value] bound to the key [qualname]. [qualname] is
    assumed to be the fully qualified name of [value].*)
let add key elt map =
  let binding =
    { value = elt;
      full_key = key;
      simple_name = ~&(Cobol_ast.qualifier_of_qualname key);
      contained_in = make_contained_in key; }
  in
  { base_keys = QualNameSet.add key map.base_keys;
    bindings = QualNameMap.add key binding map.bindings; }

(** [iter f map] iters over all the values of [map], note that the first argument of [f] is the fully
    qualified name of the element. *)
let iter f map =
  QualNameMap.iter (fun key {value; _} -> f key value) map.bindings

(** [fold f map init] folds over all the key values bindings of [map], note that the first argument
    of [f] is fully qualified name of the element. *)
let fold f map init =
  QualNameMap.fold (fun key {value; _} acc -> f key value acc) map.bindings init

