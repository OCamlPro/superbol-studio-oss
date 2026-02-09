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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

type qualrefmap = srcloc list Cobol_unit.Qual.MAP.t
type references_in_unit =
  {
    data_refs: qualrefmap;
    proc_refs: qualrefmap;
    (* TODO: const_refs, prog_refs?, others... *)
    (* TODO atm data_refs and proc_refs contains BOTH direct references and indirect references
       e.g.: DISPLAY B IN A result in
         - a direct reference to B
         - a indirect reference to A
      In the future we may need/want to split those 2 kind of references
     *)
  }
type references_in_group = references_in_unit Cobol_unit.Collections.MAP.t

type artifacts =
  {
    references: references_in_group;
  }

type outputs =
  {
    ptree: Cobol_ptree.compilation_group;
    group: Cobol_unit.Types.group;
    artifacts: artifacts;
  }
type t = outputs


(* An accumulator for fold_exec_block', used to collect references in EXEC blocks *)
type references_acc =
  {
    current_section: Cobol_unit.Types.procedure_section option;
    refs: references_in_unit;
    diags: Typeck_diagnostics.t;
  }

type fold_exec_block' =
  register_name:(string
                   Cobol_common.Srcloc.TYPES.with_loc ->
                 references_acc -> references_acc) ->
  Cobol_common.Exec_block.TYPES.exec_block
    Cobol_common.Srcloc.TYPES.with_loc ->
  references_acc -> references_acc

(* --- *)

let no_refs =
  {
    data_refs = Cobol_unit.Qual.MAP.empty;
    proc_refs = Cobol_unit.Qual.MAP.empty;
  }

let no_artifacts: artifacts =
  {
    references = Cobol_unit.Collections.MAP.empty;
  }

let none: t =
  {
    ptree = Cobol_ptree.{ control_division = None;
                          compilation_units = [] };
    group = Cobol_unit.Group.empty;
    artifacts = no_artifacts;
  }

let merge_qualrefmaps: qualrefmap -> qualrefmap -> qualrefmap =
  Cobol_unit.Qual.MAP.union (fun _ a b -> Some (b @ a))   (* keep reversed order *)

let register_qualref qn ~loc refs =
  Cobol_unit.Qual.MAP.update qn
    (function None -> Some [loc] | Some l -> Some (loc :: l)) refs

let register_data_qualref qn ~loc refs =
  { refs with data_refs = register_qualref qn ~loc refs.data_refs }

let register_proc_qualref qn ~loc refs =
  { refs with proc_refs = register_qualref qn ~loc refs.proc_refs }

let register_data_field_ref ~loc field refs =
  (* match Cobol_data.Item.qualname ~&item with *)
  match ~&field.Cobol_data.Types.field_qualname with
  | None -> refs
  | Some qn -> register_data_qualref ~&qn ~loc refs

let register_data_renaming_ref ~loc renaming refs =
  register_data_qualref
    ~&(~&renaming.Cobol_data.Types.renaming_name) ~loc refs

let register_condition_name_ref ~loc cond_name refs =
  register_data_qualref
    ~&(~&cond_name.Cobol_data.Types.condition_name_qualname) ~loc refs

(* let register_table_index_ref ~loc qn refs = *)
(*   (\* match Cobol_data.Item.qualname ~&item with *\) *)
(*   match ~&table.Cobol_data.Types.table_index with *)
(*   | None -> refs *)
(*   | Some qn -> register_data_qualref qn ~loc refs *)

let register_procedure_ref ~loc block refs =
  match Cobol_unit.Types.block_name block with
  | None -> refs
  | Some qn -> register_proc_qualref ~&qn ~loc refs
