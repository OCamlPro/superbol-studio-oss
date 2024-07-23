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

open Lsp.Types
open Cobol_common.Srcloc.INFIX
open Lsp.Types.DocumentSymbol
open Cobol_ptree
module Visitor = Cobol_common.Visitor
type range = Range.t
type doc_symbol = t

let enclosing_range (range: range) (buffer: doc_symbol list)  =
  let end_ = match buffer with
    | [] -> range.end_
    | last::_ -> last.range.end_
  in
  Range.create ~start:range.start ~end_

let create ~range ?(children=[]) =
  create ~range ~selectionRange:range ~children:(List.rev children)

type proc_acc =
  {
    result: doc_symbol list;
    prev_section: (range * string) option;
    buffer: doc_symbol list;
  }
let proc_init_acc = { result = []; prev_section = None; buffer = [] }

let complete_section { result; prev_section; buffer=children } =
  match prev_section with
  | None -> children @ result
  | Some (range, name) ->
    let range = enclosing_range range children in
    create ~kind:Function ~name ~range ~children ()
    :: result

let proc_folder range_from : proc_acc Cobol_ptree.Visitor.folder =
  object
    inherit [proc_acc] Cobol_ptree.Visitor.folder

    method! fold_procedure_division' { loc; _ } { result; _ } =
      Visitor.do_children_and_then proc_init_acc
        begin fun acc ->
          let children = complete_section acc in
          let range = range_from ~loc in
          let procedure = create ()
              ~children
              ~kind:Function
              ~range
              ~name:"PROCEDURE DIVISION"
          in
          { proc_init_acc with result = procedure::result; }
        end

    method! fold_paragraph' { loc; payload = p } acc =
      let range = range_from ~loc in
      let name = Option.fold
          ~none:"Anonymous paragraph"
          ~some:(fun s' ->
              ~&s' ^ if p.paragraph_is_section then " SECTION" else "" )
          p.paragraph_name in
      begin
        if p.paragraph_is_section
        then
          {
            buffer = [];
            prev_section = Some((range, name));
            result = complete_section acc
          }
        else
          let paragraph = create ~kind:Function ~range ~name () in
          { acc with buffer = paragraph :: acc.buffer }
      end
      |> Visitor.skip_children
  end

let fold_proc_div range_from proc_div =
  let result acc = acc.result in
  Cobol_ptree.Visitor.fold_procedure_division'
    (proc_folder range_from)
    proc_div
    proc_init_acc
  |> result

type buffered_data =
  { range: range; name: string; level: int; buffer: doc_symbol list }
type data_acc =
  { result: doc_symbol list; previous: buffered_data list; }
let data_init_acc = { result = []; previous = []; }

let create_variable { name; range; buffer=children; level } =
  ignore level;
  let range = enclosing_range range children in
  create ~children ~kind:Variable ~range ~name ()

let rec complete_entries current_level acc  =
  match acc.previous with
  | [] -> acc
  | prev::_ when current_level > prev.level -> acc
  | [prev] ->
    let prev_doc_sym = create_variable prev in
    {
      result = prev_doc_sym::acc.result;
      previous = [];
    }
  | prev::prev2::tl ->
    let prev_doc_sym = create_variable prev in
    let prev2 = { prev2 with buffer = prev_doc_sym::prev2.buffer } in
    complete_entries current_level { acc with previous = prev2::tl }

let data_folder range_from : data_acc Cobol_ptree.Visitor.folder =
  let parent_folder (type a) name ({ loc; _ }: a with_loc) acc =
    let range = range_from ~loc in
    let parent = create ~kind:Function ~range ~name in
    Visitor.do_children_and_then data_init_acc
      begin fun inner_acc ->
        let { result = children; _ } = complete_entries 0 inner_acc in
        {
          result = parent ~children () :: acc.result;
          previous = [];
        }
      end
  in
  object
    inherit [data_acc] Cobol_ptree.Visitor.folder

    method! fold_data_division' =
      parent_folder "DATA DIVISION"

    method! fold_file_section' =
      parent_folder "FILE SECTION"

    method! fold_linkage_section' =
      parent_folder "LINKAGE SECTION"

    method! fold_working_storage_section' =
      parent_folder "WORKING-STORAGE SECTION"

    method! fold_local_storage_section' =
      parent_folder "LOCAL-STORAGE SECTION"

    method! fold_communication_section' =
      parent_folder "COMMUNICATION SECTION"

    method! fold_report_section' =
      parent_folder "REPORT SECTION"

    method! fold_screen_section' =
      parent_folder "SCREEN SECTION"

    method! fold_data_item' { loc; payload = di } acc =
      let range = range_from ~loc in
      let data_name =
        match di.data_name with
        | Some { payload = DataName s'; _ } -> ~&s'
        | _ -> "FILLER"
      in
      let true_level = ~&(di.data_level) in
      let name =
        (if true_level <= 9 then "0" else "")
        ^ Int.to_string true_level
        ^ " "
        ^ data_name
      in
      let level = if true_level == 77 then 01 else true_level in
      let current = { range; name; level; buffer = [] } in
      let acc = complete_entries current.level acc in
      { acc with previous = current::acc.previous }
      |>Visitor.skip

    method! fold_rename_item' { loc; payload = ri } acc =
      let range = range_from ~loc in
      let name = "66 " ^ ~&(ri.rename_to) in
      let current = { range; name; level = 02; buffer = [] } in
      let acc = complete_entries current.level acc in
      { acc with previous = current::acc.previous }
      |>Visitor.skip

    method! fold_constant_item' { loc; payload = ci } acc =
      let range = range_from ~loc in
      let name = "01 " ^ ~&(ci.constant_name) in
      let constant = create ~kind:Constant ~range ~name () in
      let acc = complete_entries 01 acc in
      { acc with result = constant::acc.result }
      |> Visitor.skip

    method! fold_condition_name_item' { loc; payload = c } acc =
      let range = range_from ~loc in
      let name = "88 " ^ ~&(c.condition_name) in
      let condition = create ~kind:Boolean ~range ~name () in
      begin match acc.previous with
        | [] -> { acc with result = condition::acc.result }
        | prev::tl ->
          let prev = { prev with buffer = condition::prev.buffer }
          in { acc with previous = prev::tl }
      end
      |> Visitor.skip
  end

let fold_data_div range_from data_div =
  let result acc = acc.result in
  Cobol_ptree.Visitor.fold_data_division'
    (data_folder range_from)
    data_div
    data_init_acc
  |> result

type env_acc = doc_symbol list
let env_init_acc = []

let env_folder range_from : env_acc Cobol_ptree.Visitor.folder =
  let parent_folder (type a)
      ?(skip=false) name ({ loc; _ }: a Cobol_common.with_loc ) acc =
    let range = range_from ~loc in
    let parent = create ~kind:Function ~range ~name in
    if skip
    then
      Visitor.skip (parent () :: acc)
    else
      Visitor.do_children_and_then env_init_acc begin fun children ->
        parent ~children () :: acc
      end
  in
  object
    inherit [env_acc] Cobol_ptree.Visitor.folder

    method! fold_environment_division' =
      parent_folder "ENVIRONMENT DIVISION"

    method! fold_configuration_section' =
      parent_folder ~skip:true "CONFIGURATION SECTION"

    method! fold_input_output_section' =
      parent_folder ~skip:true "INPUT-OUTPUT SECTION"
  end

let fold_env_div range_from env_div =
  Cobol_ptree.Visitor.fold_environment_division'
    (env_folder range_from)
    env_div
    env_init_acc


let _every_symbol_kind _ =
  let pos = Position.create ~line:1 ~character:1 in
  let range = Range.create ~end_:pos ~start:pos in
  let create kind name = create ~kind ~name ~range () in
  [ create File "File"; create Module "Module"; create Namespace "Namespace";
    create Package "Package"; create Class "Class"; create Method "Method";
    create Property "Property"; create Field "Field";
    create Constructor "Constructor"; create Enum "Enum";
    create Interface "Interface"; create Function "Function";
    create Variable "Variable"; create Constant "Constant";
    create String "String"; create Number "Number"; create Boolean "Boolean";
    create Array "Array"; create Object "Object"; create Key "Key";
    create Null "Null"; create EnumMember "EnumMember"; create Struct "Struct";
    create Event "Event"; create Operator "Operator";
    create TypeParameter "TypeParameter";
  ]

let from_ptree_at ~uri ptree : DocumentSymbol.t list =
  let filename = Lsp.Uri.to_path uri in
  let range_from ~loc = Lsp_position.range_of_srcloc_in ~filename loc in
  let string_of_name_or_lit (name_or_lit: name_or_literal) =
    let s = Pretty.to_string "%a" pp_term name_or_lit in s
  in
  let parent_folder (type a) ~kind name ({ loc; payload }: a with_loc) acc =
      let range = range_from ~loc in
      let name =  name payload in
      Visitor.do_children_and_then []
        begin fun children ->
          create () ~children ~kind ~range ~name :: acc
        end
  in
  Cobol_ptree.Visitor.fold_compilation_group object
    inherit [doc_symbol list] Cobol_ptree.Visitor.folder

    method! fold_compilation_group _ acc =
      Visitor.do_children_and_then acc List.rev

    method! fold_program_unit' =
      parent_folder ~kind:Function
        begin fun (cu: program_unit) ->
          "PROGRAM-ID. " ^ string_of_name_or_lit ~&(cu.program_name)
        end

    method! fold_function_unit' =
      parent_folder ~kind:Function
        begin fun (fu: function_unit) ->
          "FUNCTION-ID. " ^ string_of_name_or_lit ~&(fu.function_name)
        end

    method! fold_interface_definition' =
      parent_folder ~kind:Interface
        begin fun (id: interface_definition) ->
          "INTERFACE-ID. " ^ string_of_name_or_lit ~&(id.interface_name)
        end

    method! fold_class_definition' =
      parent_folder ~kind:Class
        begin fun (c: class_definition) ->
          "CLASS-ID. " ^ string_of_name_or_lit ~&(c.class_name)
        end

    method! fold_method_definition' =
      parent_folder ~kind:Method
        begin fun (m: method_definition) ->
          "METHOD-ID. " ^ ~&(m.method_name)
        end

    method! fold_factory_definition' =
      parent_folder ~kind:Object @@ Fun.const "FACTORY."

    method! fold_instance_definition' =
      parent_folder ~kind:Object @@ Fun.const "OBJECT."

    method! fold_environment_division' div acc =
      Visitor.skip (fold_env_div range_from div @ acc)

    method! fold_data_division' div acc =
      Visitor.skip (fold_data_div range_from div @ acc)

    method! fold_procedure_division' div acc =
      Visitor.skip (fold_proc_div range_from div @ acc)
  end ptree []
