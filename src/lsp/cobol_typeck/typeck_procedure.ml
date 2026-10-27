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
open Cobol_procedure.Types
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

let baseloc_of_qualname = Typeck_references.baseloc_of_qualname

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
    let arg_name_ref, refs =
      Typeck_references.resolve_record_name arg_name acc.refs
        ~data_definitions
    in
    let arg = { arg_name_ref; arg_passing_style } &@<- arg_name in
    let acc = { rev_args = arg :: acc.rev_args; refs } in
    match arg_name_ref.resolved with
    | Error () -> acc (* Name resolution errors are already reported *)
    | Ok arg_data_definition ->
        match Cobol_data.Item.def_storage arg_data_definition with
        | Linkage -> acc
        | actual_storage ->
            (* LATER: Check dialect here since non-storage procedure arguments
               are valid in MF dialect and correspond to a callee-side copy. *)
            error acc @@ Invalid_proc_arg_storage { arg_name; actual_storage }
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
        section_list: section with_loc list;
        current_section: section_under_construction with_loc option;
        procedure_using: procedure_using with_loc option;
        refs: references_acc;
      }
    and section_under_construction =
      {
        sec_name: Cobol_ptree.name with_loc option;
        sec_segment: Cobol_data.Types.integer_value option;
        sec_paragraphs: paragraph with_loc list;
      }

    let init =
      {
        blocks = Resolver_map.empty;
        section_list = [];
        current_section = None;
        procedure_using = None;
        refs;
      }

    let procedure acc =
      {
        procedure_blocks = acc.blocks;
        procedure_sections = List.rev acc.section_list;
        procedure_using = acc.procedure_using;
      },
      acc.refs

    let name n : Cobol_ptree.qualname = Name n
    let qual n q : Cobol_ptree.qualname = Qual (n, q)

    let finish_section
        ({ payload = suc; loc }: section_under_construction with_loc) =
      let section_paragraphs =
        (* The section is completely empty: add an empty paragraph to ensure
           that each section has at least one paragraph *)
        if suc.sec_paragraphs = [] then
          let empty_paragraph : paragraph = {
            paragraph_name = None;
            paragraph_sentences = [] }
          in
          { named = Resolver_map.empty;
            list = [empty_paragraph &@ loc] }
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
      { section_name = suc.sec_name;
        section_segment = suc.sec_segment;
        section_paragraphs } &@ loc

    let commit_section acc =
      match acc.current_section with
      | Some ({ payload = { sec_name; _ }; _ } as section) ->
          let section = finish_section section in
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

    let typeck_paragraph ~paragraph_name (p: Cobol_ptree.paragraph) acc =
      let paragraph_sentences, refs =
        Typeck_statements.typeck_sentences p.paragraph_sentences
          Typeck_procedure_monad.{ data_definitions } acc.refs
      in
      let acc = { acc with refs } in
      { paragraph_name; paragraph_sentences }, acc

    let start_new_section (p: Cobol_ptree.paragraph with_loc) acc =
      let n = ~&p.paragraph_name in
      let acc = commit_section acc in
      let sec_segment, refs =
        Typeck_procedure_monad.map_option
          (Typeck_terms.typeck_integer ~loc:~@p) ~&p.paragraph_segment
          Typeck_procedure_monad.{ data_definitions } acc.refs
      in
      let acc = { acc with refs } in
      let sec_paragraphs, acc =
        (* Avoid adding an empty section header paragraph if unneeded, and
           remove section name from the location of the unnamed paragraph at
           the begining of section. *)
        if ~&p.paragraph_sentences = [] then
          [], acc
        else
          let header_paragraph, acc = typeck_paragraph ~paragraph_name:None ~&p acc in
          let loc = Option.get (Cobol_common.Srcloc.concat_locs ~&p.paragraph_sentences) in
          [header_paragraph &@ loc], acc
      in
      { acc with
        current_section = Some ({ sec_name = n;
                                  sec_segment;
                                  sec_paragraphs } &@<- p) }

    let add_paragraph_in_current_section (p: Cobol_ptree.paragraph with_loc) acc =
      let s, loc = match acc.current_section with
        | None -> { sec_name = None; sec_segment = None; sec_paragraphs = [] }, ~@p
        | Some s -> ~&s, Cobol_common.Srcloc.concat ~@s ~@p
      in
      let paragraph_name = match ~&p.paragraph_name, s.sec_name with
        | None, _ -> None (* For first paragraph of PROCEDURE DIVISION *)
        | Some n, None -> Some (name n &@<- n)
        | Some n, Some sn -> Some (qual n (name sn) &@<- n)
      in
      let typed_paragraph, acc = typeck_paragraph ~paragraph_name ~&p acc in
      let p = typed_paragraph &@<- p in
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
      let procedure_using, refs =
        resolve_procedure_args ~data_definitions ~refs:acc.refs args
      in
      Visitor.skip_children { acc with procedure_using; refs }

    method! fold_paragraph' p acc =
      Visitor.skip_children @@
      if ~&p.paragraph_is_section then
        start_new_section p acc
      else
        add_paragraph_in_current_section p acc

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

    method! fold_section ({ section_paragraphs; _ } as s)
        ({ current_section; _ } as acc) =
      let acc =
        Visitor.fold_list v section_paragraphs.list
          ~fold:Cobol_unit.Visitor.fold_paragraph'
          { acc with current_section = Some s }
      in
      Visitor.skip { acc with current_section }

    method! fold_paragraph { paragraph_sentences; _ } acc =
      Visitor.skip @@
      Visitor.fold_list v paragraph_sentences
        ~fold:Cobol_procedure.Visitor.fold_statements' acc

    method! fold_procedure_name' qn
        ({ current_section = enclosing_section; _ } as acc) =
      let register ?enclosing_section qn acc =
        let loc = baseloc_of_qualname ~&qn in
        match Cobol_unit.Procedure.find ~&qn ?enclosing_section procedure with
        | block ->
            { acc with
              refs = Typeck_outputs.register_procedure_ref ~loc block acc.refs }
        | exception Not_found ->
            error acc @@ Unknown_proc_name qn
        | exception Resolver_map.Ambiguous (lazy matching_qualnames) ->
            error acc @@ Ambiguous_proc_name { given_qualname = qn;
                                               matching_qualnames }
      in
      let acc = register ?enclosing_section qn acc in
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
