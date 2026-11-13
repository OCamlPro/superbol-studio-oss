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
open Lsp_imports
open Lsp_lookup.TYPES

let lookup_qn ~kind ~lookup qn =
  try Some (lookup qn) with
  | Not_found ->
      Lsp_notify.unknown kind qn;
      None
  | Cobol_unit.Resolver_map.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous kind qn ~matching_qualnames;
      None

let find_full_qn ~kind qn qmap =
  lookup_qn ~kind qn
    ~lookup:(fun qn -> (Cobol_unit.Resolver_map.find_binding qn qmap).full_qn)

let find_proc_qn ~kind qn ?enclosing_section cu =
  lookup_qn ~kind qn
    ~lookup:begin fun qn ->
      Cobol_unit.Procedure.full_qn ?enclosing_section qn
        cu.Cobol_unit.Types.unit_procedure
    end

let ppenv_var_reference_locs (loc_translator: Lsp_position.translator)
    ~(doc: Lsp_document.t)
    (ppvar_def: Cobol_preproc.Env.var_definition) =
  List.filter_map begin fun (event: Cobol_preproc.Trace.log_entry) ->
    match ppvar_def, event with
    | Compilation_var compvar_def,
      (* | Variable_definition _ / filter out definition locs *)
      (Variable_substitution { loc; def; _ } |
       Variable_evaluation { loc; def = Some Compilation_var def; _ }) ->
        if def == compvar_def (* Note: assumes sharing btw map and log entries *)
        then Some (loc_translator.location_of_srcloc loc)
        else None
    | Preproc_var ppvar_def,
      Variable_evaluation { loc; def = Some Preproc_var def; _ } ->
        if def == ppvar_def   (* Note: assumes sharing btw map and log entries *)
        then Some (loc_translator.location_of_srcloc loc)
        else None
    | _ ->
        None
  end (Cobol_preproc.Trace.events doc.artifacts.pplog)

(** [locations ~doc loc_translator ?cu_name element references] gives the
    locations of all references to [element] (excluding its definition), using
    the [references] computed by the typechecker for [doc].  [cu_name] is the
    name of the compilation unit that encloses [element]. *)
let locations ~(doc: Lsp_document.t)
    (Lsp_position.{ location_of_srcloc; _ } as loc_translator)
    ?cu_name element (references: Cobol_typeck.Outputs.references_in_group) =
  let data_refs (cu_refs: Cobol_typeck.Outputs.references_in_unit) qn =
    List.rev_map location_of_srcloc
      (Cobol_unit.Qual.MAP.find qn cu_refs.data_refs)
  and proc_refs (cu_refs: Cobol_typeck.Outputs.references_in_unit) qn =
    List.rev_map location_of_srcloc
      (Cobol_unit.Qual.MAP.find qn cu_refs.proc_refs)
  in
  let[@local] with_cu_n_refs f =
    match cu_name with
    | None ->
        []
    | Some cu_name ->
        try f @@ CUMap.find_by_name cu_name references
        with Not_found -> []
  in
  match element with
  | Data_full_name qn
  | Data_item { full_qn = Some qn; _ } ->
      Lsp_debug.message "Lsp_references.find_references: \
                         Data_full_name...";
      with_cu_n_refs @@ fun (_cu, cu_refs) ->
      data_refs cu_refs qn
  | Data_item { full_qn = None; _ } ->
      Lsp_debug.message "Lsp_references.find_references: \
                         Data_item...";
      []
  | Data_name qn ->
      Lsp_debug.message "Lsp_references.find_references: \
                         Data_name...";
      with_cu_n_refs @@ fun (cu, cu_refs) ->
      Option.fold ~none:[] ~some:(data_refs cu_refs) @@
      find_full_qn qn ~&cu.unit_data.data_items.named ~kind:"data-name"
  | Proc_name { qn; enclosing_section } ->
      Lsp_debug.message "Lsp_references.find_references: \
                         Proc_name...";
      with_cu_n_refs @@ fun (cu, cu_refs) ->
      Option.fold ~none:[] ~some:(proc_refs cu_refs) @@
      find_proc_qn qn ?enclosing_section ~&cu ~kind:"procedure-name"
  | Preproc_or_compilation_variable_ref { def; _ } ->
      ppenv_var_reference_locs ~doc loc_translator def
