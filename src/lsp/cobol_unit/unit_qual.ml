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

open EzCompat                                                    (* StringSet *)
module StrSet = StringSet

open Cobol_common.Srcloc.INFIX

(* --- *)

let pp = Cobol_ptree.pp_qualname
let compare = Cobol_ptree.compare_qualname

let name n : Cobol_ptree.qualname = Name n

let name_of : Cobol_ptree.qualname -> string = function
  | Qual (n, _) | Name n -> String.uppercase_ascii ~&n

let qual_of : Cobol_ptree.qualname -> _ option = function
  | Qual (_, qn) -> Some qn | Name _ -> None

let qual name : Cobol_ptree.qualname option -> Cobol_ptree.qualname = function
  | None -> Name name
  | Some qn -> Qual (name, qn)

(** [requal qn qn'] qualifies [qn] with [qn'] iff [qn] is not already
    qualified. *)
let requal: (Cobol_ptree.qualname as 'a) -> 'a option -> 'a = fun qn qn' ->
  let rec aux (qn: Cobol_ptree.qualname) = match qn with
    | Name name -> qual name qn'
    | Qual (name, qn) -> Qual (name, aux qn)
  in
  match qn with
  | Name _ -> aux qn
  | Qual _ -> qn

let names_of : Cobol_ptree.qualname -> StrSet.t =
  let rec aux acc : Cobol_ptree.qualname -> StrSet.t = function
    | Name _ as n -> StrSet.add (name_of n) acc
    | Qual (_, qn) as n -> aux (StrSet.add (name_of n) acc) qn
  in
  aux StrSet.empty

let indirect_quals_of : Cobol_ptree.qualname -> StrSet.t = function
  | Name _ -> StrSet.empty
  | Qual (_, qn) -> names_of qn

let rec matches (qn: Cobol_ptree.qualname) ~(full: Cobol_ptree.qualname) =
  match qn, full with
  | Name _, Name _ ->
      name_of qn = name_of full
  | Qual _, Name _ ->
      false
  | Qual (_, qn'), Qual (_, full')
    when name_of qn = name_of full ->
      matches qn' ~full:full'
  | qn, Qual (_, full') ->
      matches qn ~full:full'

(** {1 Collections} *)

(** Collections to be used over fully qualified names. *)

module M = struct
  type t = Cobol_ptree.qualname
  let compare = Cobol_ptree.compare_qualname
end

module SET = Stdlib.Set.Make (M)
module MAP = Stdlib.Map.Make (M)
