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

let register_data_qualref ~loc full_qn acc =
  { acc with refs = Typeck_outputs.register_data_qualref ~loc full_qn acc.refs }

(** Registers the references to the qualifiers of [qn], e.g, [A] in [B IN A]
    (see {!Typeck_outputs.references_in_unit}).  Qualifiers are looked up in
    [full_qn], the fully qualified name of the item that [qn] references, if
    known; otherwise, they are resolved on their own, and silently ignored if
    this resolution fails: only [qn] needs to be unambiguous. *)
let rec register_qualifiers ~data_definitions ?full_qn
    (qn: Cobol_ptree.qualname) acc =
  match qn with
  | Name _ ->
      acc
  | Qual (_, qn') ->
      let full_qn =
        match Option.bind full_qn Cobol_unit.Qual.qual_of with
        | Some full_qn -> Cobol_unit.Qual.suffix_named qn' full_qn
        | None ->
            try
              Some (Resolver_map.find_binding qn'
                      data_definitions.data_items.named).full_qn
            with Not_found | Resolver_map.Ambiguous _ -> None
      in
      Option.fold full_qn ~none:acc ~some:(fun full_qn ->
          register_data_qualref ~loc:(baseloc_of_qualname qn') full_qn acc)
      |> register_qualifiers ~data_definitions ?full_qn qn'

(** Emits a warning diagnostic on undefined data-name unless
    [assume_partial_data_definitions] is explictly set to [true].
    The reference is registered at the location of the base name of [qn], along
    with references to its qualifiers. *)
let resolve_data_qualname ~data_definitions
    ?(assume_partial_data_definitions = false)
    qn acc =
  let resolved, acc, full_qn =
    match Resolver_map.find_binding ~&qn data_definitions.data_items.named with
    | bnd ->
        let loc = baseloc_of_qualname ~&qn in
        Ok bnd.value,
        register_data_qualref ~loc bnd.full_qn acc,
        Some bnd.full_qn
    | exception Not_found ->
        Error (),
        (if assume_partial_data_definitions
         then acc                              (* skip "undefined" diagnostic *)
         else error acc @@ Undefined_data_name qn),
        None
    | exception Resolver_map.Ambiguous (lazy matching_qualnames) ->
        Error (),
        error acc @@ Ambiguous_data_name { given_qualname = qn;
                                           matching_qualnames },
        None
  in
  { resolved_name = qn; resolved },
  register_qualifiers ~data_definitions ?full_qn ~&qn acc

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
          | _ -> Error (), error acc @@ Not_a_toplevel_record { name; data_def }
        end
    | Ok data_def -> Error (), error acc @@ Not_a_toplevel_record { name; data_def }
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
