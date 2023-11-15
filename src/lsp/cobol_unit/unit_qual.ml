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

(* open Cobol_common.Srcloc.TYPES *)
open Cobol_common.Srcloc.INFIX

let name n : Cobol_ptree.qualname = Name n

let name_of : Cobol_ptree.qualname -> string = function
  | Qual (n, _) | Name n -> ~&n

let qual_of : Cobol_ptree.qualname -> _ option = function
  | Qual (_, qn) -> Some qn | Name _ -> None

let qual name : Cobol_ptree.qualname option -> Cobol_ptree.qualname = function
  | None -> Cobol_ptree.Name name
  | Some qn -> Cobol_ptree.Qual (name, qn)

let rec requal: Cobol_ptree.qualname -> Cobol_ptree.qualname option ->
  Cobol_ptree.qualname = fun qn qn' -> match qn with
  | Name name -> qual name qn'
  | Qual (name, qn) -> Qual (name, requal qn qn')

let names_of : Cobol_ptree.qualname -> StrSet.t =
  let rec aux acc : Cobol_ptree.qualname -> StrSet.t = function
    | Name n -> StrSet.add ~&n acc
    | Qual (n, qn) -> aux (StrSet.add ~&n acc) qn
  in
  aux StrSet.empty

let indirect_quals_of : Cobol_ptree.qualname -> StrSet.t = function
  | Name _ -> StrSet.empty
  | Qual (_, qn) -> names_of qn

let compare = Cobol_ptree.compare_qualname

let rec matches (qn: Cobol_ptree.qualname) ~(full: Cobol_ptree.qualname) =
  match qn, full with
  | Name n, Name n' -> ~&n = ~&n'
  | Qual _, Name _ -> false
  | Qual (n, qn), Qual (n', qn') when ~&n = ~&n' -> matches qn ~full:qn'
  | qn, Qual (_, qn') -> matches qn ~full:qn'
