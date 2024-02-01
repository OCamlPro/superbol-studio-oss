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

module Visitor = Cobol_common.Visitor

module TYPES = struct

  (** Type definitions to help representing and querying (partially) parsed
      COBOL programs. *)


  (** Information returned by {!val:Lookup.element_at_position}. *)
  type element_at_position =
    {
      element_at_position: element_in_context option;
      enclosing_compilation_unit_name: string option;
    }
  and element_in_context =
    | Data_name of
        Cobol_ptree.Types.qualname
    | Data_full_name of
        Cobol_ptree.Types.qualname
    | Data_item of
        {
          full_qn: Cobol_ptree.Types.qualname option;
          def_loc: srcloc;
        }
    | Proc_name of
        {
          qn: Cobol_ptree.Types.qualname;
          in_section: Cobol_unit.Types.procedure_section option;
        }

  type name_definition =
    {
      as_paragraph: paragraph_definition option;   (* link to AST's paragraph *)
      as_item: item_definition option;
    }
  and paragraph_definition =
    Cobol_ptree.Types.paragraph with_loc
  and item_definition =
    Cobol_data.Types.item_definition with_loc

  type copy_operation = string with_loc            (** with loc of [COPY ...] *)

end
open TYPES

(* --- *)

let baseloc_of_qualname: Cobol_ptree.Types.qualname -> srcloc = function
  | Name name
  | Qual (name, _) -> ~@name

(* TODO: manipulate positions that are combined with a filename instead of
   passing `filename` around all the time. *)

(** [lexloc_of_qualname_in ~filename qualname] returns the full location of
    [qualname], from the first qualifier to the last.  This function is
    temporary and is expected to be replaced once a better way of getting this
    location is found. *)
let lexloc_of_qualname_in ~filename (qn: Cobol_ptree.Types.qualname) =
  let rec end_pos: Cobol_ptree.Types.qualname -> Lexing.position = function
    | Name n -> Cobol_common.Srcloc.end_pos_in ~filename ~@n
    | Qual (_, qn) -> end_pos qn
  in
  match qn with
  | Name n -> Cobol_common.Srcloc.lexloc_in ~filename ~@n
  | Qual (n, qn) -> Cobol_common.Srcloc.start_pos_in ~filename ~@n, end_pos qn

(** [qualname_at_pos ~filename qualname pos] returns the qualname built from all
    the qualifiers of [qualname] that are after or at position [pos] ion
    [filename].  This function is temporary and is expected to be replaced once
    a better way of finding the qualname is implemented. *)
let rec qualname_at_pos ~filename (qn: Cobol_ptree.Types.qualname) pos =
  match qn with
  | Name _ ->
      qn
  | Qual (name, qn') ->
      try
        let lexloc = lexloc_of_qualname_in ~filename qn in
        if not (Position.is_after_lexloc pos lexloc) &&
           not (Position.is_in_srcloc ~filename pos ~@name)
        then qualname_at_pos ~filename qn' pos
        else qn
      with Invalid_argument _ -> qn                               (* dummy loc *)

(* --- *)

(** [element_at_position pos group] seeks the compilation unit name and qualified
    name at the given position [pos], in typed compilation group [group]. *)
let element_at_position ~uri pos group : element_at_position =
  let filename = Lsp.Uri.to_path uri in
  let open struct

    type acc =
      {
        elt: element_at_position;
        context: context;
      }
    and context =
      | Data_decls
      | Procedure of Cobol_unit.Types.procedure_section option

    let init =
      {
        elt = { element_at_position = None;
                enclosing_compilation_unit_name = None };
        context = Data_decls;                       (* does not really matter *)
      }

    let result acc = acc.elt

  end in

  let enter_context context ({ context = prev_context; _ } as acc) =
    Visitor.do_children_and_then { acc with context }
      (fun acc -> { acc with context = prev_context })
  in

  let on_data_full_name qn ({ elt; _ } as acc) =
    { acc with
      elt = { elt with element_at_position = Some (Data_full_name qn) } }
  and on_data_name qn ({ elt; _ } as acc) =
    { acc with
      elt = { elt with element_at_position = Some (Data_name qn) } }
  and on_data_item ?full_qn def_loc ({ elt; _ } as acc) =
    { acc with
      elt = { elt with element_at_position = Some (Data_item { full_qn;
                                                               def_loc }) } }
  and on_proc_name qn ({ elt; context } as acc) =
    let element_at_position = match context with
      | Data_decls -> Some (Proc_name { qn; in_section = None })   (* unlikely *)
      | Procedure in_section -> Some (Proc_name { qn; in_section })
    in
    { acc with elt = { elt with element_at_position } }
  in

  Cobol_unit.Visitor.fold_unit_group object
    inherit [acc] Cobol_unit.Visitor.folder
    inherit! [acc] Position.sieve ~filename ~pos

    method! fold_cobol_unit' cu ({ elt; _ } as acc) =
      let name = ~&(~&cu.unit_name) in
      Visitor.do_children @@
      { acc with
        elt = { elt with enclosing_compilation_unit_name = Some name } }

    method! fold_data_definitions _ = enter_context Data_decls
    method! fold_procedure _ = enter_context (Procedure None)
    method! fold_procedure_section s = enter_context (Procedure (Some s))

    method! fold_field_definition' def acc =
      Visitor.do_children @@ match ~&def.field_qualname with
      | None -> on_data_item ~@def acc                               (* FILLER *)
      | Some qn -> on_data_item ~full_qn:~&qn ~@def acc

    method! fold_record_renaming' ren acc =
      Visitor.do_children @@
      on_data_item ~full_qn:~&(~&ren.renaming_name) ~@ren acc

    method! fold_qualname qn acc =
      let qn = qualname_at_pos ~filename qn pos in
      Visitor.skip_children @@ match acc.context with
      | Data_decls ->            (* always fully qualified in data definitions *)
          on_data_full_name qn acc
      | Procedure _ ->            (* for now, no more info, data-name expected *)
          on_data_name qn acc

    method! fold_procedure_name qn acc =
      Visitor.skip_children @@
      on_proc_name (qualname_at_pos ~filename qn pos) acc

  end group init |> result

(* --- *)

let copy_at_pos ~filename pos ptree =
  Cobol_ptree.Visitor.fold_compilation_group object
    inherit [copy_operation option] Cobol_ptree.Visitor.folder
    method! fold' { loc; _ } = function
      | Some _ as acc ->
          Visitor.skip_children acc
      | None ->
          match Cobol_common.Srcloc.as_copy loc with
          | Some { loc; _ } as copy
            when Position.is_in_srcloc ~filename pos loc ->
              Visitor.skip_children copy
          | _ ->
              Visitor.do_children None
  end ptree None
