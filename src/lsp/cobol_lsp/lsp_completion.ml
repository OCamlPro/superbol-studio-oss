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
  | Boolean _ -> Any

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
  | Index
  | Program_pointer _ -> Numeric
  | National _ -> Alphanum
  | Object_reference _
  | Bit _ -> Any
  | Display pic -> approx_type_of_pic pic

let approx_type_of_datadef : data_definition -> (approx_typing_info * bool) = fun d ->
  match d with
  | Data_field { def; _ } -> begin
      match ~&def.field_layout with
      | Elementary_field { usage; _ } -> (approx_type_of_usage usage, false)
      | Struct_field _ -> (Alphanum, true)
    end
  | Data_renaming { def; _} -> begin
      match ~&def.renaming_layout with
      | Renamed_elementary { usage } -> (approx_type_of_usage usage, false)
      | Renamed_struct _ -> (Alphanum, true)
    end
  | Data_condition _
  | Table_index _ ->
    (Any, false)

let is_valid ~comp_categories data =
  let (data_cat, is_group) = approx_type_of_datadef data in
  List.exists
    begin fun cat ->
      cat == data_cat ||
      cat == Any ||
      (is_group && cat == Group)
    end
    comp_categories

let qualnames_proposal ~filename pos group : (string * bool) list =
  let comp_categories = Lsp_lookup.type_at_pos ~filename pos group in
  match Lsp_lookup.last_cobol_unit_before_position ~filename pos group with
  | None -> []
  | Some cu ->
    cu.unit_data.data_items.list
    |> List.filter_map begin fun d ->
      Option.map (fun qn -> qn, is_valid ~comp_categories d)
      @@ Cobol_data.Item.def_qualname d
    end
    |> List.rev_map begin fun (qn, valid) ->
      List.map (fun s -> s, valid) @@ to_string qn end
    |> List.flatten

let procedures_proposal ~filename pos group =
  match Lsp_lookup.last_cobol_unit_before_position ~filename pos group with
  | None -> []
  | Some cu ->
    let paragraph_name (paragraph:Cobol_unit.Types.procedure_paragraph with_loc) =
      Option.map Cobol_common.Srcloc.payload ~&paragraph.paragraph_name
    in
    List.flatten @@ List.rev_map (function
        | Cobol_unit.Types.Paragraph paragraph ->
          Option.to_list @@ paragraph_name paragraph
        | Section section ->
          List.filter_map paragraph_name ~&section.section_paragraphs.list)
      cu.unit_procedure.list
    |> List.rev_map to_string
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

let completion_item_create ?(detail="") ?(delay=false) ~range ~kind ~case text=
  let text = change ~case text in
  let textedit = TextEdit.create ~newText:text ~range in
  let labelDetails = CompletionItemLabelDetails.create ~detail () in
  CompletionItem.create ()
    ~labelDetails
    ~label:text ~kind ~preselect:false
    ~sortText:(if delay then "zz" ^ text else text)
    ~textEdit:(`TextEdit textedit)

let string_of_K tokens =
  let pp ppf =
    let rec inner ?(space_before=true) = function
      | [] -> ()
      | Cobol_parser.Tokens.PERIOD::tl ->
        Fmt.string ppf ".\n"; inner tl
      | hd::tl ->
        let token' = hd &@ Srcloc.dummy in
        if space_before then Fmt.sp ppf ();
        Cobol_parser.INTERNAL.pp_token ppf token'; (* TODO: Cobol_parser.Tokens.pp *)
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
        |> List.rev_map begin fun (s, valid) ->
          completion_item_create
            ~delay:(not valid)
            ~kind:Variable ~range ~case s
            ~detail:(if valid then "" else " wrong type")
        end
      | ProcedureRef ->
        procedures_proposal ~filename pos group
        |> List.rev_map (completion_item_create
                           ~kind:Function ~range ~case)
      | FunctionName ->
        all_intrinsic_function_name
        |> List.rev_map (completion_item_create
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

let context_completion_list ~config
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
    | _ -> CompletionList.create () ~isIncomplete:true ~items:[]
  end

