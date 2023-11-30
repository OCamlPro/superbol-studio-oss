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

open EzCompat                                              (* String{Map,Set} *)
module StrMap = StringMap
module StrSet = StringSet

open Unit_qual
module NEL = Cobol_common.Basics.NEL

(* --- *)

module TYPES = struct

  type 'a qualmap =
    {
      map: 'a node StrMap.t;
      indirect_quals: StrSet.t;            (* indirect qualifiers used in map *)
    }

  and 'a node =
    | Exact of {
        binding: 'a binding option;
        refined: 'a qualmap;
      }
    | Cut of {
        binding: 'a binding;
        qn_suffix: Cobol_ptree.qualname;
      }

  and 'a binding =
    {
      value: 'a;
      full_qn: Cobol_ptree.qualname;    (* kept so we have distinct locations *)
    }

  exception Ambiguous of Cobol_ptree.qualname NEL.t Lazy.t

end
include TYPES

type +'a t = 'a qualmap

(* --- *)

let is_empty { map; _ } =
  StrMap.is_empty map

let fold ~f map acc =
  let rec fold_map { map; _ } acc =
    StrMap.fold begin fun _ node acc ->
      match node with
      | Exact { binding = None; refined } ->
          fold_map refined acc
      | Exact { binding = Some binding; refined } ->
          f binding acc |> fold_map refined
      | Cut { binding; _ } ->
          f binding acc
    end map acc
  in
  fold_map map acc

let bindings map =
  fold ~f:List.cons map []

let binding_qualifiers bindings =
  List.map (fun b -> b.full_qn) bindings

let pp_qualname ppf qn =
  Pretty.print ppf "@[<h>%a@]" Cobol_ptree.pp_qualname qn

let pp_binding pp_value ppf { value; full_qn; _ } =
  Pretty.print ppf "@[<h 2>%a@ =>@ %a@]"
    pp_qualname full_qn
    pp_value value

let pp_qualmap pp_value ppf map =
  Pretty.list ~fopen:"{@;<1 2>" ~fsep:"@;<1 2>" ~fclose:"@;}" ~fempty:"{}"
    (pp_binding pp_value) ppf
    (bindings map)

let pp_qualmap_struct pp_value ppf map =
  let rec pp_map ppf { map; _ } =
    Pretty.list ~fopen:"{|@;<1 2>" ~fsep:"@;<1 2>" ~fclose:"@;|}" ~fempty:"{/}"
      (fun ppf (key, node) -> Pretty.print ppf "@[<hv 2>%S:@ %a@]" key pp_node node)
      ppf (StrMap.bindings map)
  and pp_node ppf = function
    | Exact { binding; refined } ->
        Pretty.print ppf "@[<hv 2>Exact {@;binding = %a,@;refined = %a@]@;}"
          Fmt.(option ~none:(any "None") (pp_binding pp_value))
          binding pp_map refined
    | Cut { binding; qn_suffix } ->
        Pretty.print ppf "@[<hv 2>Cut {@;%a,@;suffix = %a@]@;}"
          (pp_binding pp_value) binding pp_qualname qn_suffix
  in
  pp_map ppf map

let rec find_binding qn { map; _ } =
  match StrMap.find (name_of qn) map, qual_of qn with
  | Cut { binding; _ }, None
  | Exact { binding = Some binding; _ }, None ->
      binding
  | Exact { binding = None; refined; _ }, None ->
      find_unique_binding qn refined
  | Cut { binding; qn_suffix }, Some qn'
    when Unit_qual.matches qn' ~full:qn_suffix ->
      binding
  | Cut _, Some _ ->
      raise Not_found
  | Exact { refined; _ }, Some qn' ->
      (* Find every binding in [refined], possibly skipping several
         qualification levels: *)
      match find_all_bindings qn' refined with
      | [binding] ->
          binding
      | [] ->
          raise Not_found
      | bindings ->
          raise @@ Ambiguous (lazy (NEL.of_list @@ binding_qualifiers bindings))
      (* | exception Ambiguous _ -> raise @@ Ambiguous qn (\* raise with proper qn *\) *)

and find_unique_binding qn ({ map; _ } as qmap) =
  if StrMap.cardinal map > 1
  then raise @@ Ambiguous (lazy (NEL.of_list @@
                                 binding_qualifiers @@ bindings qmap))
  else match snd @@ StrMap.choose map with            (* [Not_found] if empty *)
    | Cut { binding; _ } ->
        binding
    | Exact { binding = Some binding; refined }
      when is_empty refined ->
        binding
    | Exact { binding = None; refined } ->
        find_unique_binding qn refined
    | Exact { refined; _ } ->
        raise @@ Ambiguous (lazy (NEL.of_list @@
                                  binding_qualifiers @@ bindings refined))

and find_all_bindings qn qmap : _ binding list =
  let rec aux ({ indirect_quals; _ } as qmap) acc =
    let acc = acc_direct_binding qmap acc in
    if StrSet.mem (name_of qn) indirect_quals
    then acc_indirect_bindings qmap acc
    else acc
  and acc_direct_binding qmap acc =
    try find_binding qn qmap :: acc with Not_found -> acc
  and acc_indirect_bindings { map; _ } acc =
    (* Skip keys at toplevel of map: *)
    StrMap.fold begin fun _key node acc -> match node with
      | Cut { binding; qn_suffix }
        when Unit_qual.matches qn ~full:qn_suffix ->
          binding :: acc
      | Cut _ ->
          acc
      | Exact { refined; _ } ->
          aux refined acc
    end map acc
  in
  aux qmap []

let find given_qn map =
  (find_binding given_qn map).value

let empty: _ qualmap =
  {
    map = StrMap.empty;
    indirect_quals = StrSet.empty;
  }

let add full_qn value (map: 'a qualmap) =
  let rec insert qn binding { map; indirect_quals } =
    let map =
      StrMap.update (name_of qn) begin fun prev_node ->
        Option.some @@ match prev_node, qual_of qn with
        | None, Some qn' ->
            Cut { binding;
                  qn_suffix = qn' }
        | None, None ->
            Exact { binding = Some binding;
                    refined = empty }
        | Some Exact { refined; _ }, None ->                        (* replace *)
            Exact { binding = Some binding;
                    refined }
        | Some Exact { binding = b0; refined }, Some qn' ->
            Exact { binding = b0;
                    refined = insert qn' binding refined }
        | Some Cut { binding = cut_binding; qn_suffix }, None ->
            Exact { binding = Some binding;
                    refined = insert qn_suffix cut_binding empty }
        | Some Cut { binding = cut_binding; qn_suffix }, Some qn' ->
            Exact { binding = None;
                    refined = (insert qn_suffix cut_binding @@
                               insert qn' binding empty) }
      end map
    and indirect_quals =
      StrSet.union indirect_quals (indirect_quals_of qn)
    in
    { map; indirect_quals }
  in
  insert full_qn { value; full_qn } map
