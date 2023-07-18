(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2023 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cobol_common
open Cobol_common.Srcloc.INFIX

type folding_range = {
  startLine:int;
  endLine:int;
  startCharacter:int;
  endCharacter:int;
  (* kind:Lsp.Types.FoldingRangeKind.t option *)
  (* collapsedText:string option*)
}

let folding_range_of_loc loc =
  match Srcloc.as_lexloc loc with
  | (* None -> None *)
  (* | Some *) (p1, p2) ->
      Some {
        startLine = p1.pos_lnum - 1;
        startCharacter = p1.pos_cnum - p1.pos_bol;
        endLine = p2.pos_lnum - 1;
        endCharacter = p2.pos_cnum - p2.pos_bol
      }

let add_folding_range r l =
  match r with
  | None -> l
  | Some r -> r :: l

let add_folding_range_of_loc loc l =
  add_folding_range (folding_range_of_loc loc) l


let folding_range_division ast =

  let visitor = Cobol_parser.PTree_visitor.fold_compilation_group (object
    inherit [folding_range list] Cobol_parser.PTree_visitor.folder

    method! fold_program_unit' {loc; _} acc =
      Visitor.do_children @@
        add_folding_range_of_loc loc acc

    method! fold_procedure_division' {loc; _} acc =
      Visitor.skip_children @@
        add_folding_range_of_loc loc acc

    method! fold_data_division' {loc; _} acc =
      Visitor.skip_children @@
        add_folding_range_of_loc loc acc

    (*TODO: add location for some nodes in the ast
            so we can define folding_range for
            environment division, file section... (predefined section)*)
    (* method! fold_environment_division' {loc; _} acc =
      Visitor.skip_children @@
        let folding_range = folding_range_of_loc loc in
        add_folding_range folding_range acc *)
  end) in
  visitor ast []


let folding_range_paragraph ast =
  let update_section_range loc (section_range, l) =
    match folding_range_of_loc loc with
    | None -> section_range, l
    | Some ({endLine; endCharacter; _} as r) ->
        Option.map (
          fun folding_range->
            {folding_range with endLine; endCharacter}
        ) section_range, r :: l
  in

  let add_section (r, l) = add_folding_range r l in

  let visitor = Cobol_parser.PTree_visitor.fold_compilation_group (object
    inherit [folding_range option *
             folding_range list] Cobol_parser.PTree_visitor.folder

    method! fold_paragraph' {payload = {paragraph_is_section; _}; loc} acc =
      Visitor.skip_children @@
        if not paragraph_is_section then
          update_section_range loc acc
        else
          folding_range_of_loc loc,
          add_section acc

    end) in
  add_section @@ visitor ast (None, [])


(*TODO:
  Now we use the result of Cobol_typeck (need to be rewritten),
  which does not work for renames-item, condition-item ... *)
let folding_range_data ({cu_wss; _}:Cobol_data.Types.compilation_unit) =
  let update r group_range =
    match r with
    | None -> group_range
    | Some {endLine; endCharacter; _} ->
        Option.map (
          fun folding_range ->
            {folding_range with endLine; endCharacter}
        ) group_range
  in
  (*add the folding_range of grouped item *)
  let rec add group l =
    let r = folding_range_of_loc ~@group in
    match ~&group with
    | Cobol_data.Group.Elementary _
    | Constant _ | Renames _ | ConditionName _ -> None, l
    | Group {elements; _} ->
        let r, l =
          List.fold_left
            (fun (r, l) group -> aux group (r, l))
            (r, l) elements
        in
        match r with
        | None -> None, l
        | Some r -> Some r, r :: l
  (*traverse the elements, update the folding_range of grouped item*)
  and aux group (r, l) =
    match ~&group with
    | Cobol_data.Group.Elementary _
    | Constant _ | Renames _ | ConditionName _ ->
        update (folding_range_of_loc ~@group) r, l
    | Group _ ->
        let r', l = add group l in
        update r' r, l
  in
  Pretty.error "%a @." Cobol_data.Group.pp_data_group_list cu_wss;
  List.fold_left
    (fun acc group -> snd @@ add group acc) [] cu_wss


let folding_range Lsp_document.TYPES.{ast; cus; _ }=
  folding_range_paragraph ast @ folding_range_division ast @
  ( Cobol_data.Compilation_unit.SET.to_seq cus
    |> Seq.map (fun cu -> folding_range_data cu)
    |> List.of_seq
    |> List.flatten)
