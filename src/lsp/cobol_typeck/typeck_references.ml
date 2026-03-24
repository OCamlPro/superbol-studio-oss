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

open Typeck_outputs                                     (* for references_acc *)
open Typeck_procedure_diagnostics
open Cobol_unit.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Qualmap = Cobol_unit.Qualmap

let empty_accumulator =
  {
    current_section = None;
    refs = Typeck_outputs.no_refs;
    diags = Typeck_diagnostics.none;
  }

let error acc err =
  { acc with diags = Proc_error err :: acc.diags }

(** May raise {!Not_found} when [strict = true] (the default) *)
let resolve_data_qualname ~data_definitions ?(strict = true)
    ({ loc; payload = qn } as qn') acc =
  try
    let bnd = Qualmap.find_binding qn data_definitions.data_items.named in
    Ok { resolved_name = qn'; resolved = bnd.value },
    { acc with
      refs = Typeck_outputs.register_data_qualref ~loc bnd.full_qn acc.refs }
  with
  | Not_found when not strict ->
      (* TODO: error acc @@ Undefined_data_item { qualname = qn' } *)
      Error (),
      acc
  | Qualmap.Ambiguous (lazy matching_qualnames) ->
      Error (),
      error acc @@ Ambiguous_data_name { given_qualname = qn &@ loc;
                                         matching_qualnames }

(** May raise {!Not_found} *)
let resolve_record_name ~data_definitions name acc =
  let res, acc =
    resolve_data_qualname (Cobol_ptree.Name name &@<- name) acc
      ~data_definitions
  in
  Result.map (fun { resolved; _ } -> { resolved; resolved_name = name }) res,
  acc

let register_data_qualname ~data_definitions qn' acc =
  snd @@
  resolve_data_qualname ~data_definitions qn' acc
    ~strict:false  (* ignore missing defs unfil we process all the DATA
                      DIVISION. *)

let register_name ~data_definitions name acc =
  register_data_qualname (Cobol_ptree.Name name &@<- name) acc
    ~data_definitions
