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

module Visitor = Cobol_common.Visitor
open Cobol_common.Srcloc.INFIX
open Lsp.Types.DocumentSymbol
module Kind = Lsp.Types.SymbolKind
type range = Lsp.Types.Range.t
type doc_symbol = t

let enclosing_range (range: range) (buffer: doc_symbol list)  =
  let end_ = match buffer with
    | [] -> range.end_
    | last::_ -> last.range.end_
  in
  Lsp.Types.Range.create ~start:range.start ~end_

let create ~range ?(children=[]) =
  create ~range ~selectionRange:range ~children:(List.rev children)

module type T = sig
  type t
  val fold:
    (loc:Cobol_common.srcloc -> range)
    -> t
    -> doc_symbol list
end

module Proc
  : T with type t = Cobol_ptree.procedure_division Cobol_common.with_loc
= struct
  type t = Cobol_ptree.procedure_division Cobol_common.with_loc
  type acc =
    {
      result: doc_symbol list;
      prev_section: (range * string) option;
      buffer: doc_symbol list;
    }

  let result acc = acc.result
  let init_acc = { result = []; prev_section = None; buffer = [] }

  let complete_section { result; prev_section; buffer=children } =
    match prev_section with
    | None -> children @ result
    | Some (range, name) ->
      let range = enclosing_range range children in
      create ~kind:Function ~name ~range ~children ()
      :: result

  let folder range_from: acc Cobol_ptree.Visitor.folder =
    object
      inherit [acc] Cobol_ptree.Visitor.folder

      method! fold_procedure_division' { loc; _ } { result; _ } =
        Visitor.do_children_and_then init_acc
          begin fun acc ->
            let children = complete_section acc in
            let range = range_from ~loc in
            let procedure = create ()
                ~children
                ~kind:Module
                ~range
                ~name:"PROCEDURE DIVISION"
            in
            { init_acc with result = procedure::result; }
          end

      method! fold_paragraph' { loc; payload = p } acc =
        let range = range_from ~loc in
        let name = Option.fold
            ~none:"Unnamed paragraph"
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

  let fold range_from proc_div =
    Cobol_ptree.Visitor.fold_procedure_division'
      (folder range_from)
      proc_div
      init_acc
    |> result
end

module Data
  : T with type t = Cobol_ptree.data_division Cobol_common.with_loc
= struct
  type t = Cobol_ptree.data_division Cobol_common.with_loc
  type buffered_data =
    { range: range; name: string; level: int; buffer: doc_symbol list }
  type acc =
    { result: doc_symbol list; previous: buffered_data list; }
  let result acc = acc.result
  let init_acc = { result = []; previous = []; }

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

  let folder range_from : acc Cobol_ptree.Visitor.folder =
    let parent_folder: type a. _ =
      fun name ({ loc; _ }: a Cobol_ptree.with_loc) acc ->
      let range = range_from ~loc in
      let parent = create ~kind:Module ~range ~name in
      Visitor.do_children_and_then init_acc
        begin fun inner_acc ->
          let { result = children; _ } = complete_entries 0 inner_acc in
          {
            result = parent ~children () :: acc.result;
            previous = [];
          }
        end
    in
    object
      inherit [acc] Cobol_ptree.Visitor.folder

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
          | Some { payload = Cobol_ptree.DataName s'; _ } -> ~&s'
          | _ -> "FILLER"
        in
        let true_level = ~&(di.data_level) in
        let name =
          (if true_level <= 9 then "0" else "")
          ^ Int.to_string true_level
          ^ " "
          ^ data_name
        in
        let level = if true_level == 77 then 01 else true_level
        in
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

  let fold range_from data_div =
    Cobol_ptree.Visitor.fold_data_division'
      (folder range_from)
      data_div
      init_acc
    |> result
end

module Env
  : T with type t = Cobol_ptree.environment_division Cobol_common.with_loc
= struct
  type t = Cobol_ptree.environment_division Cobol_common.with_loc
  type acc = doc_symbol list
  let init_acc = []

  let folder range_from: acc Cobol_ptree.Visitor.folder =
    let parent_folder: type a. _ =
      fun ?(skip=false) name ({ loc; _ }: a Cobol_common.with_loc ) acc ->
      let range = range_from ~loc in
      let parent = create ~kind:Module ~range ~name in
      if skip
      then
        Visitor.skip (parent () :: acc)
      else
      Visitor.do_children_and_then init_acc
        begin fun children -> parent ~children () :: acc end
    in
    object
      inherit [acc] Cobol_ptree.Visitor.folder

      method! fold_environment_division' =
        parent_folder "ENVIRONMENT DIVISION"

      method! fold_configuration_section' =
        parent_folder ~skip:true "CONFIGURATION SECTION"

      method! fold_input_output_section' =
        parent_folder ~skip:true "INPUT-OUTPUT SECTION"
    end

  let fold range_from env_div =
    Cobol_ptree.Visitor.fold_environment_division'
      (folder range_from)
      env_div
      init_acc
end

let retrieve ~uri ptree : Lsp.Types.DocumentSymbol.t list =
  let filename = Lsp.Uri.to_path uri in
  let range_from ~loc = Lsp_position.range_of_srcloc_in ~filename loc in
  let string_of_name_or_lit (name_or_lit: Cobol_ptree.name_or_literal) =
    let s = Pretty.to_string "%a" Cobol_ptree.pp_term name_or_lit in s
  in
  let parent_folder: type a. _ =
    fun ~kind name ({ loc; payload }: a Cobol_ptree.with_loc) acc ->
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
      parent_folder ~kind:Module
      begin fun (cu: Cobol_ptree.program_unit) ->
      "PROGRAM-ID. " ^ string_of_name_or_lit ~&(cu.program_name)
      end

    method! fold_function_unit' =
      parent_folder ~kind:Function
      begin fun (fu: Cobol_ptree.function_unit) ->
      "FUNCTION-ID. " ^ string_of_name_or_lit ~&(fu.function_name)
      end

    method! fold_interface_definition' =
      parent_folder ~kind:Interface
      begin fun (id: Cobol_ptree.interface_definition) ->
      "INTERFACE-ID. " ^ string_of_name_or_lit ~&(id.interface_name)
      end

    method! fold_class_definition' =
      parent_folder ~kind:Class
      begin fun (c: Cobol_ptree.class_definition) ->
      "CLASS-ID. " ^ string_of_name_or_lit ~&(c.class_name)
      end

    method! fold_method_definition' =
      parent_folder ~kind:Method
      begin fun (m: Cobol_ptree.method_definition) ->
      "METHOD-ID. " ^ ~&(m.method_name)
      end

    method! fold_factory_definition' =
      parent_folder ~kind:Object begin Fun.const "FACTORY." end

    method! fold_instance_definition' =
      parent_folder ~kind:Object begin Fun.const "OBJECT." end

    method! fold_environment_division' div acc =
      let result = Env.fold range_from div in
      Visitor.skip (result @ acc)

    method! fold_data_division' div acc =
      let result = Data.fold range_from div in
      Visitor.skip (result @ acc)

    method! fold_procedure_division' div acc =
      let result = Proc.fold range_from div in
      Visitor.skip (result @ acc)
  end ptree []
