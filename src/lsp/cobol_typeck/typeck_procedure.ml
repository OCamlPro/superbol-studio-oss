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

open Typeck_outputs                                     (* for references_acc *)
open Typeck_procedure_diagnostics
open Cobol_unit.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Resolver_map = Cobol_unit.Resolver_map
module Visitor = Cobol_common.Visitor

type output =
  {
    procedure: Cobol_unit.Types.procedure;
    references: Typeck_outputs.references_in_unit;
  }

let baseloc_of_qualname: Cobol_ptree.qualname -> srcloc = function
  | Name name
  | Qual (name, _) -> ~@name

let error acc err =
  { acc with diags = Proc_error err :: acc.diags }

let resolve_procedure_args ~data_definitions ~refs pa =
  let open struct
    type procedure_args_accumulator =
      {
        rev_args: procedure_arg with_loc list;
        refs: references_acc;
      }
  end in

  let error acc err = { acc with refs = error acc.refs err } in

  let resolve_arg_name_ref ~arg_name ~arg_passing_style acc =
    let register_linkage_record ~arg_data_definition acc =
      match Cobol_data.Item.def_storage arg_data_definition.resolved with
      | Linkage ->
          { acc with
            rev_args = ({ arg_data_definition;
                          arg_passing_style } &@<- arg_name) :: acc.rev_args }
      | actual_storage ->
          error acc @@ Invalid_proc_arg_storage { arg_name; actual_storage }
    in
    match
      (* CHECKME: may need to only lookup among records (maybe also only in
         LINKAGE), not among every data def. *)
      let arg_data_definition, refs =
        Typeck_references.resolve_record_name arg_name acc.refs
          ~data_definitions
      in
      { acc with refs }, arg_data_definition
    with
    | acc, Error () ->
        acc                                                   (* skip arg def *)
    | acc, Ok arg_data_definition ->
        register_linkage_record ~arg_data_definition acc
    | exception Not_found ->
        error acc @@ Procedure_arg_record_not_found { arg_name }
  in

  Cobol_ptree.Visitor.fold_procedure_using_phrase object
    inherit [procedure_args_accumulator] Cobol_ptree.Visitor.folder
    method! fold_procedure_calling_style _ = Visitor.skip
    method! fold_procedure_by_value_arg a acc =
      Visitor.skip_children @@
      resolve_arg_name_ref acc
        ~arg_name:a.by_value_arg_name
        ~arg_passing_style:Arg_by_value
    method! fold_procedure_by_reference_arg a acc =
      let optional = a.by_reference_arg_optional in
      Visitor.skip_children @@
      resolve_arg_name_ref acc
        ~arg_name:a.by_reference_arg_name
        ~arg_passing_style:(Arg_by_reference { optional })
  end ~&pa { rev_args = []; refs } |>
  fun acc -> Some (List.rev acc.rev_args &@<- pa), acc.refs


let procedure_of_compilation_unit ~data_definitions ~refs cu' =

  let open struct
    type acc =
      {
        blocks: procedure_block Resolver_map.t;
        section_list: procedure_section with_loc list;
        current_section: section_under_construction with_loc option;
        args: Cobol_ptree.procedure_using_phrase with_loc option;
      }
    and section_under_construction =
      {
        sec_name: Cobol_ptree.name with_loc option;
        sec_paragraphs: procedure_paragraph with_loc list;
      }

    let init =
      {
        blocks = Resolver_map.empty;
        section_list = [];
        current_section = None;
        args = None;
      }

    let procedure_using acc =
      match acc.args with
      | None ->
          None, refs
      | Some args ->
          resolve_procedure_args ~data_definitions ~refs args

    let procedure acc =
      let procedure_using, references = procedure_using acc in
      {
        procedure_blocks = acc.blocks;
        procedure_sections = List.rev acc.section_list;
        procedure_using;
      },
      references

    let name n : Cobol_ptree.qualname = Name n
    let qual n q : Cobol_ptree.qualname = Qual (n, q)

    let procedure_section
        ({ payload = suc; loc }: section_under_construction with_loc) =
      let section_paragraphs =
        (* The section is completely empty: add an empty paragraph to ensure 
           that each section has at least one paragraph *)
        if suc.sec_paragraphs = [] then
          let empty_paragraph = Cobol_ptree.{ 
            paragraph_name = suc.sec_name;
            paragraph_is_section = true; 
            paragraph_segment = None; 
            paragraph_sentences = [] }
          in
          { named = Resolver_map.empty; 
            list = [{ paragraph_name = None; paragraph = empty_paragraph &@ loc} &@ loc] }
        else
          List.fold_left (fun ({ named; list } as paragraphs) paragraph ->
            match ~&paragraph.paragraph_name with
            | None ->
                { paragraphs with list = paragraph :: list }
            | Some qn ->
                { named = Resolver_map.add ~&qn paragraph named;
                  list = paragraph :: list }
            ) { named = Resolver_map.empty; list = [] } suc.sec_paragraphs
      in
      { section_name = suc.sec_name; section_paragraphs } &@ loc

    let commit_section acc =
      match acc.current_section with
      | Some ({ payload = { sec_name; _ }; _ } as section) ->
          let section = procedure_section section in
          let blocks = match sec_name with
            | Some sec_name -> Resolver_map.add (name sec_name) (Section section) acc.blocks
            | None -> acc.blocks
          in
          { acc with
            blocks;
            section_list = section :: acc.section_list;
            current_section = None }
      | None ->
          acc

    let start_new_section p acc =
      let n = Cobol_ptree.(~&p.paragraph_name) in
      let acc = commit_section acc in
      let sec_paragraphs =
        (* Avoid adding an empty section header paragraph if unneeded, and
           remove section name from the location of the unnamed paragraph at
           the begining of section. *)
        match Cobol_common.Srcloc.concat_locs ~&p.paragraph_sentences with
        | None -> [] (* when ~&p.paragraph_sentences = [] *)
        | Some loc -> [{ paragraph_name = None; paragraph = p } &@ loc] 
      in
      { acc with
        current_section = Some ({ sec_name = n;
                                  sec_paragraphs } &@<- p) }

    let paragraph_in_current_section (p: Cobol_ptree.paragraph with_loc) acc =
      let s, loc = match acc.current_section with
        | None -> { sec_name = None; sec_paragraphs = [] }, ~@p
        | Some s -> ~&s, Cobol_common.Srcloc.concat ~@s ~@p
      in
      let paragraph_name = match ~&p.paragraph_name, s.sec_name with
        | None, _ -> None (* For first paragraph of PROCEDURE DIVISION *)
        | Some n, None -> Some (name n &@<- n)
        | Some n, Some sn -> Some (qual n (name sn) &@<- n)
      in
      let p = { paragraph_name; paragraph = p } &@<- p in
      let blocks = match paragraph_name with
        | None -> acc.blocks
        | Some qn -> Resolver_map.add ~&qn (Paragraph p) acc.blocks
      in
      { acc with
        blocks;
        current_section = Some ({ s with sec_paragraphs = p :: s.sec_paragraphs } &@ loc) }

  end in

  let visitor = object
    inherit [acc] Cobol_ptree.Visitor.folder

    method! fold_informational_paragraphs _ = Visitor.skip
    method! fold_options_paragraph' _ = Visitor.skip
    method! fold_environment_division _ = Visitor.skip
    method! fold_data_division' _ = Visitor.skip
    method! fold_procedure_division _ acc =
      Visitor.do_children_and_then acc
        commit_section                  (* be sure to handle the last section *)

    method! fold_procedure_using_phrase' args acc =
      Visitor.skip_children { acc with args = Some args }

    method! fold_paragraph' p acc =
      Visitor.skip_children @@ 
        if ~&p.paragraph_is_section then
          start_new_section p acc 
        else 
          paragraph_in_current_section p acc

    method! fold_nested_programs _  = Visitor.skip                    (* TODO *)
  end in

  Cobol_ptree.Visitor.fold_compilation_unit' visitor
    cu' init |> procedure

(* --- *)

let collect_references
    ~(data_definitions: Cobol_unit.Types.data_definitions)
    ~(fold_exec_block': Typeck_outputs.exec_block_folder)
    ~(refs: references_acc)
    procedure =

  let visitor = object (v)
    inherit [references_acc] Cobol_unit.Visitor.folder

    method! fold_qualname qn acc =                (* TODO: data_name' instead *)
      Visitor.do_children @@
      Typeck_references.register_data_qualname
        ~data_definitions
        (qn &@ baseloc_of_qualname qn) acc

    method! fold_procedure_section ({ section_paragraphs; _ } as s)
        ({ current_section; _ } as acc) =
      let acc =
        Visitor.fold_list v section_paragraphs.list
          ~fold:Cobol_unit.Visitor.fold_procedure_paragraph'
          { acc with current_section = Some s }
      in
      Visitor.skip { acc with current_section }

    method! fold_procedure_paragraph { paragraph; _ } acc =
      Visitor.skip @@
      Cobol_ptree.Proc_division_visitor.fold_paragraph' v paragraph acc

    method! fold_procedure_name' qn
        ({ current_section = in_section; _ } as acc) =
      let register ?in_section qn acc =
        let loc = baseloc_of_qualname ~&qn in
        match Cobol_unit.Procedure.find ~&qn ?in_section procedure with
        | block ->
            { acc with
              refs = Typeck_outputs.register_procedure_ref ~loc block acc.refs }
        | exception Not_found ->
            error acc @@ Unknown_proc_name qn
        | exception Resolver_map.Ambiguous (lazy matching_qualnames) ->
            error acc @@ Ambiguous_proc_name { given_qualname = qn;
                                               matching_qualnames }
      in
      let acc = register ?in_section qn acc in
      let acc = match ~&qn with
        | Name _ -> acc
        | Qual (_, section_qn) ->
            register (section_qn &@ baseloc_of_qualname section_qn) acc
      in
      Visitor.skip_children acc

    method! fold_exec_block' exec_block acc =
      Visitor.skip_children @@
      fold_exec_block' ~data_definitions exec_block acc

  end in

  Cobol_unit.Visitor.fold_procedure visitor procedure refs

(* --- *)

let of_compilation_unit ~data_definitions ~fold_exec_block' cu' =
  let procedure, refs =
    procedure_of_compilation_unit ~data_definitions cu'
      ~refs:Typeck_references.empty_accumulator
  in
  let { refs = references; diags; _ } =
    collect_references ~data_definitions ~fold_exec_block' ~refs procedure
  in
  { procedure; references }, diags
