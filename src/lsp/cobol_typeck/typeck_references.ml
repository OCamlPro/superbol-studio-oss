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
open Cobol_procedure.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Resolver_map = Cobol_unit.Resolver_map

let empty_accumulator =
  {
    current_section = None;
    refs = Typeck_outputs.no_refs;
    diags = Typeck_diagnostics.none;
  }

let error acc err =
  { acc with diags = Proc_error err :: acc.diags }

let baseloc_of_qualname: Cobol_ptree.qualname -> srcloc = function
  | Name name
  | Qual (name, _) -> ~@name

(** Emits a warning diagnostic on undefined data-name unless
    [assume_partial_data_definitions] is explictly set to [true].
    The reference is registered at the location of the base name of [qn]. *)
let resolve_data_qualname ~data_definitions
    ?(assume_partial_data_definitions = false)
    qn acc =
  try
    let bnd = Resolver_map.find_binding ~&qn data_definitions.data_items.named in
    let loc = baseloc_of_qualname ~&qn in
    { resolved_name = qn; resolved = Ok bnd.value },
    { acc with
      refs = Typeck_outputs.register_data_qualref ~loc bnd.full_qn acc.refs }
  with
  | Not_found ->
      { resolved_name = qn; resolved = Error () },
      if assume_partial_data_definitions
      then acc                                 (* skip "undefined" diagnostic *)
      else error acc @@ Undefined_data_name qn
  | Resolver_map.Ambiguous (lazy matching_qualnames) ->
      { resolved_name = qn; resolved = Error () },
      error acc @@ Ambiguous_data_name { given_qualname = qn;
                                         matching_qualnames }

let resolve_record_name ~data_definitions ?(assume_partial_data_definitions = false)
    name acc =
  let res, acc =
    resolve_data_qualname (Cobol_ptree.Name name &@<- name) acc
      ~data_definitions ~assume_partial_data_definitions
  in
  let resolved, acc = match res.resolved with
    | Error () -> Error (), acc
    | Ok (Data_field { record; def; _ } as data_def) ->
        begin match ~&(record.record_item) with
          | Field root_field when ~&def == root_field -> Ok record, acc
          | Table root_table when ~&def == ~&(root_table.table_field) -> Ok record, acc
          | _ -> Error (), error acc @@ Subfield_where_record_expected { name; data_def }
        end
    | Ok data_def -> Error (), error acc @@ Subfield_where_record_expected { name; data_def }
  in
  { resolved; resolved_name = name },
  acc

let register_data_qualname ~data_definitions qn' acc =
  snd @@
  resolve_data_qualname ~data_definitions qn' acc
    ~assume_partial_data_definitions:true (* ignore missing defs until we
                                             process all the DATA DIVISION. *)

let register_name ~data_definitions name acc =
  register_data_qualname (Cobol_ptree.Name name &@<- name) acc
    ~data_definitions
