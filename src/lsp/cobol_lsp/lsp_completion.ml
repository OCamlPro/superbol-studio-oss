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

open EzCompat

open Cobol_common                                                  (* Visitor, NEL *)
open Cobol_common.Srcloc.INFIX

open Lsp.Types
open Lsp_lookup.TYPES
open Cobol_data.Types

module Menhir = Cobol_parser.Grammar_interpr
module Expect = Cobol_parser.Expect

type actual_case =
  | Uppercase
  | Lowercase

type case =
  | Auto
  | Uppercase
  | Lowercase

let detect_case word : actual_case =
  let case_score = String.fold_left (fun acc ch ->
      if Char.lowercase ch == ch then (acc-1) else (acc+1)) 0 word
  in
  if case_score >= 0
  then Uppercase
  else Lowercase

let actual_case: case -> string -> actual_case =
  function
  | Auto -> detect_case
  | Lowercase -> fun _ -> Lowercase
  | Uppercase -> fun _ -> Uppercase

let change ~(case: actual_case) = match case with
  | Uppercase -> String.uppercase
  | Lowercase -> String.lowercase

let to_string = function
  | Cobol_ptree.Name name -> [~&name]
  | Qual (name,_) as qualname ->
    [~&name; Pretty.to_string "%a" Cobol_ptree.pp_qualname qualname]
  | _ -> []

let approx_type_to_string = function
  | Alphanum -> "Alphanum"
  | Any -> ""
  | Boolean -> "Boolean"
  | Condition -> "Condition"
  | Group -> "Group"
  | Index -> "Index"
  | NumericEdited -> "NumericEdited"
  | Numeric -> "Numeric"
  | ObjectRef -> "Object Ref"
  | Pointer -> "Pointer"

let approx_type_of_pic ({ category; _ }: picture) =
  match category with
  | National _ | Alphabetic _ | Alphanumeric _ -> Alphanum
  | FixedNum { editions; _ } when (editions.basics <> [] ||
                                   editions.floating <> None ||
                                   editions.zerorepl <> None)
    -> NumericEdited
  | FloatNum { editions; _ } when editions <> []
    -> NumericEdited
  | FloatNum _ | FixedNum _ -> Numeric
  | Boolean _ -> Boolean

let approx_type_of_usage : usage -> approx_typing_info = function
  | Binary _
  | Binary_C_long _
  | Binary_char _
  | Binary_double _
  | Binary_long _
  | Binary_short _
  | Float_long
  | Float_short
  | Float_binary _
  | Float_decimal _
  | Float_extended
  | Packed_decimal _ -> Numeric
  | Procedure_pointer
  | Function_pointer _
  | Pointer _
  | Program_pointer _ -> Pointer
  | Index -> Index
  | National _ -> Alphanum
  | Object_reference _ -> ObjectRef
  | Bit _ -> Boolean
  | Display pic -> approx_type_of_pic pic

let approx_type_of_datadef : data_definition -> (approx_typing_info * bool) =
  function
  | Data_field
      { def = { payload = {
            field_layout = Elementary_field { usage; _ };
            _ }; _ }; _ }
  | Data_renaming
      { def = { payload = {
            renaming_layout = Renamed_elementary { usage; _ };
            _ }; _ }; _ } ->
    (approx_type_of_usage usage, false)
  | Data_field
      { def = { payload = {
            field_layout = Struct_field _;
            _ }; _ }; _ }
  | Data_renaming
      { def = { payload = {
            renaming_layout = Renamed_struct _;
            _ }; _ }; _ } ->
    (Alphanum, true)
  | Data_condition _ -> (Condition, false)
  | Table_index _ -> (Index, false)

let is_valid ~expected data =
  let (data_cat, is_group) = approx_type_of_datadef data in
  List.exists
    begin fun cat ->
      cat == data_cat ||
      cat == Any ||
      (is_group && cat == Group)
    end
    expected

type typed_qualname = {
  name: string;
  typ: approx_typing_info;
  is_valid: bool;
}

let qualnames_proposal ~filename pos group : typed_qualname list =
  let expected_approx_types = Lsp_lookup.type_at_pos ~filename pos group in
  match Lsp_lookup.last_cobol_unit_before_pos ~filename pos group with
  | None -> []
  | Some cu ->
    cu.unit_data.data_items.list
    |> List.filter_map begin fun d ->
      let (typ, is_group) = approx_type_of_datadef d in
      let typ = if is_group then Group else typ in
      Option.map (fun qn -> qn, typ, is_valid ~expected:expected_approx_types d)
      @@ Cobol_data.Item.def_qualname d
    end
    |> List.rev_map begin fun (qn, typ, is_valid) ->
      List.map (fun name -> { name; typ; is_valid; }) @@ to_string qn end
    |> List.flatten

let procedures_proposal ~filename pos group =
  match Lsp_lookup.last_cobol_unit_before_pos ~filename pos group with
  | None -> []
  | Some cu ->
    let to_string_with_type typ paragraph =
      match ~&paragraph with
      | Cobol_unit.Types.{ paragraph_name = Some { payload = qn; _ }; _ } ->
        List.map (fun s -> typ, s) @@ to_string qn
      | _ -> []
    in
    cu.unit_procedure.list
    |> List.rev_map begin function
      | Cobol_unit.Types.Paragraph p ->
        to_string_with_type "Paragraph" p
      | Section section ->
        List.mapi begin fun i paragraph ->
          let typ = (if i == 0 then "Section" else "Paragraph") in
          to_string_with_type typ paragraph end
          ~&section.section_paragraphs.list
        |> List.flatten
    end
    |> List.flatten

let all_intrinsic_function_name =
  List.map fst Cobol_parser.Keywords.intrinsic_functions

module CompEntrySet = Set.Make(Expect.Completion_entry)

let get_nthline s n =
  let rec inner line_start i : int * int =
    let line_end =
      try String.index_from s line_start '\n'
      with Not_found | Invalid_argument _ -> String.length s in
    if i >= n
    then (line_start, line_end)
    else inner (line_end+1) (i+1)
  in
  let (start,end_) = inner 0 0 in
  String.sub s start (end_-start)

let word_delimiters = [' '; '\t'; '.'; '('; ')']
let rec first_delimiter_index_before text idx =
  if idx == 0 then 0 else
  if List.mem (String.get text (idx-1)) word_delimiters
  then idx
  else first_delimiter_index_before text (idx-1)

let range_n_case case (pos:Position.t) text =
  let { line; character = caret_column }: Position.t = pos in
  let text_line = get_nthline (Lsp.Text_document.text text) line in
  let word_start_column = first_delimiter_index_before text_line caret_column in
  let position_start = Position.create ~character:word_start_column ~line in
  let start_of_word =
    String.sub text_line word_start_column (caret_column - word_start_column) in
  Range.create ~start:position_start ~end_:pos,
  actual_case case start_of_word

let p_highest = 3
let p_high = 2
let p_low = 1
let p_none = 0

let completion_item_create ?(detail="") ?(priority_sort=p_none) ~range ~kind ~case text=
  let text = change ~case text in
  let textEdit =`TextEdit (TextEdit.create ~newText:text ~range) in
  let sortText = String.init priority_sort (Fun.const '.') ^ text in
  CompletionItem.create ()
    ?detail:(if detail == "" then None else Some detail)
    ~label:text ~kind ~preselect:false
    ~sortText
    ~textEdit

let string_of_K tokens =
  let pp ppf =
    let rec inner ?(space_before=true) = function
      | [] -> ()
      | Cobol_parser.Tokens.PERIOD::tl ->
        Fmt.string ppf ".\n"; inner tl
      | token::tl ->
        if space_before then Fmt.sp ppf ();
        Cobol_parser.Tokens.pp ppf token;
        inner tl
    in
    inner ~space_before:false
  in
  Pretty.to_string "%a" pp (Basics.NEL.to_list tokens)

let map_completion_items ~(range:Range.t) ~case ~group ~filename comp_entries =
  let pos = range.end_ in
  List.flatten @@ EzList.tail_map (function
      | Expect.Completion_entry.QualifiedRef ->
        qualnames_proposal ~filename pos group
        |> List.rev_map begin fun { name; typ; is_valid } ->
          let typ = approx_type_to_string typ in
          completion_item_create
            ~priority_sort:(if is_valid then p_high else p_low)
            ~detail:(if is_valid then typ else typ ^ " (unexpected here)")
            ~kind:Variable ~range ~case name
        end
      | ProcedureRef ->
        procedures_proposal ~filename pos group
        |> List.rev_map begin fun (typ, name) ->
          completion_item_create
            ~detail:typ
            ~priority_sort:p_highest
            ~kind:Function ~range ~case name
        end
      | FunctionName ->
        all_intrinsic_function_name
        |> List.rev_map (completion_item_create
                           ~detail:"Intrinsic"
                           ~kind:Function ~range ~case)
      | K tokens -> begin
          try [ completion_item_create ~kind:Keyword ~range ~case @@
                string_of_K tokens ]
          with Not_found -> [] end)
    (CompEntrySet.elements comp_entries)

let expected_comp_entries_in ~env ~eager =
  List.to_seq @@
  if eager
  then Expect.eager_completion_entries_in ~env
  else Expect.completion_entries_in ~env

let expected_tokens ?(eager=true) base_env =
  let rec inner acc env  =
    let pos = match Menhir.top env with
      | None -> snd (Srcloc.as_lexloc Srcloc.dummy)
      | Some Menhir.Element (_, _, _, pos) -> pos in
    let acc =
      CompEntrySet.add_seq (expected_comp_entries_in ~env ~eager) acc in
    Expect.actions_in ~env
    |> List.filter_map begin function
      | Expect.Reduce prod -> Some ( Menhir.force_reduction prod env )
      | Feed nt ->
        try
          let default_value = Expect.default_nonterminal_value nt in
          Some ( Menhir.feed (N nt) pos default_value pos env )
        with Not_found -> None
    end
    |> List.fold_left inner acc
  in
  inner CompEntrySet.empty base_env

type config =
  {
    eager: bool;
    case: case;
  }

let config ?(eager=true) ?(case=Auto) () =
  {
    eager;
    case;
  }

let contextual ~config
    (doc:Lsp_document.t)
    Cobol_typeck.Outputs.{ group; _ }
    (pos:Position.t) =
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri doc.textdoc) in
  let range, case = range_n_case config.case pos doc.textdoc in
  let pointwise = range.start.character == range.end_.character in
  begin match Lsp_document.inspect_at ~position:(range.start) doc with
    | Some Env env ->
      let items =
        map_completion_items ~range ~case ~group ~filename
        @@ expected_tokens ~eager:config.eager env
      in
      CompletionList.create () ~isIncomplete:pointwise ~items
    | _ ->
      CompletionList.create () ~isIncomplete:true ~items:[]
  end

