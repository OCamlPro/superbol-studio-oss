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

module Menhir = Cobol_parser.Grammar_interpr
module Expect = Cobol_parser.Expect

let qualname_proposals ~filename pos group =
  match Lsp_lookup.cobol_unit_at_position ~filename pos group with
  | None -> []
  | Some cu ->
    List.filter_map Cobol_data.Item.def_qualname cu.unit_data.data_items.list

let procedure_proposals ~filename pos group =
  match Lsp_lookup.cobol_unit_at_position ~filename pos group with
  | None -> []
  | Some cu ->
      let map_paragraph (paragraph:Cobol_unit.Types.procedure_paragraph with_loc) =
        Option.to_list @@ Option.map Cobol_common.Srcloc.payload ~&paragraph.paragraph_name
      in
      List.flatten @@ List.map (function
        | Cobol_unit.Types.Paragraph paragraph -> map_paragraph paragraph
        | Section section ->
            List.flatten @@ List.map map_paragraph ~&section.section_paragraphs.list)
      cu.unit_procedure.list

let completion_item label ~range ~kind =
    (*we may change the ~sortText/preselect for reason of priority *)
    let textedit = TextEdit.create ~newText:label ~range in
    CompletionItem.create ()
      ~label ~kind ~preselect:false
      ~textEdit:(`TextEdit textedit)

let delimiters = [' '; '\t'; '.']
let range (pos:Position.t) text =
  let { line; character }: Position.t = pos in
  let get_nthline s n =
    let rec inner line_start i : int * int =
      let line_end = try String.index_from s line_start '\n' with _ -> String.length s in
      if i >= n
        then (line_start, line_end)
        else inner (line_end+1) (i+1)
      in
      let (start,end_) = inner 0 0 in
      String.sub s start (end_-start)
      in
    let text_line = get_nthline (Lsp.Text_document.text text) line in
    let index = List.fold_left (fun acc delimiter ->
      let current =
        try 1 + String.rindex_from text_line (character - 1) delimiter
        with _ -> 0 in
      max acc current) 0 delimiters in
    let position_start = Position.create ~character:index ~line in
    Range.create ~start:position_start ~end_:pos

let to_completion_item ~kind ~range qualnames =
  List.flatten @@ List.map (function
    | Cobol_ptree.Name name -> [~&name]
    | Qual (name,_) as qualname ->
        [~&name; Pretty.to_string "%a" Cobol_ptree.pp_qualname qualname]
    | _ -> []) qualnames |>
    List.map @@ completion_item ~kind ~range

let string_of_K tokens =
  let pp ppf =
    let rec inner ?(space_before=true) = function
      | [] -> ()
      | hd::tl ->
          let token' = hd &@ Srcloc.dummy in
          if hd <> Cobol_parser.Tokens.PERIOD && space_before then Fmt.sp ppf ();
          Cobol_parser.INTERNAL.pp_token ppf token';
          inner tl
    in inner ~space_before:false
  in Pretty.to_string "%a" pp (Basics.NEL.to_list tokens)

let map_completion_items ~(range:Range.t) ~group ~filename comp_entries =
      let pos = range.end_ in
      List.flatten @@ List.map (function
        | Expect.QualifiedRef ->
            to_completion_item
              ~kind:CompletionItemKind.Variable ~range
              (qualname_proposals ~filename pos group)
        | ProcedureRef ->
            to_completion_item
            ~kind:CompletionItemKind.Module ~range
            (procedure_proposals ~filename range.end_ group)
        | K tokens -> begin
            try let tokens_string = string_of_K tokens in
              [completion_item ~kind:CompletionItemKind.Keyword ~range tokens_string]
            with Not_found -> [] end)
      (Expect.CompEntrySet.elements comp_entries)

let pp_env ppf env = (* for debug *)
  let has_default = Expect.reducible_productions_in ~env <> [] in
  Fmt.pf ppf "%d%s" (Menhir.current_state_number env) (if has_default then "_" else "")

let pp_env_stack ppf env = (* for debug *)
  let rec get_stack env =
    match Menhir.pop env with
      | None -> [env]
      | Some popped_env -> env::(get_stack popped_env)
    in
    Fmt.pf ppf "TOP(%a)  STACK: %a" pp_env env
    (Fmt.list ~sep:(Fmt.any " ") pp_env) (get_stack env)

let debug = false
let expected_tokens base_env =
  let rec inner env acc =
    let pos = match Menhir.top env with
      | None -> snd (Srcloc.as_lexloc Srcloc.dummy)
      | Some Menhir.Element (_, _, _, pos) -> pos in
    if debug then (Lsp_io.log_debug "In State: %a" pp_env env; let tok = Expect.acceptable_terminals_in ~env in if List.length tok > 0 then Lsp_io.log_debug "Gained %d entries [%a]" (List.length tok) Fmt.(list ~sep:(any ";") Expect.pp_completion_entry) tok);
    let productions = Expect.reducible_productions_in ~env in
    let nullables = Expect.acceptable_nullable_nonterminals_in ~env in
    let acc = Expect.CompEntrySet.add_seq
      (List.to_seq @@ Expect.acceptable_terminals_in ~env) acc in
    let acc = List.fold_left (fun acc (Menhir.X sym) ->
      let default_value =
        try Some (Cobol_parser.Recover.default_value sym)
        with Not_found -> try Some (Expect.guessed_default_value_of_nullables sym)
        with Not_found -> if debug then  Lsp_io.log_debug "Not found"; None in
      Option.fold ~none:acc ~some:(fun a ->
        let new_env = Menhir.feed sym pos a pos env in
        if debug then Lsp_io.log_debug "NULLABLES: On %a\nGot to %a" pp_env_stack env pp_env_stack new_env;
        inner new_env acc) default_value)
    acc nullables in
    List.fold_left (fun acc prod ->
      let new_env = Menhir.force_reduction prod env in
      if debug then Lsp_io.log_debug "Force prod %d, on %a\nGot to %a" (Menhir.production_index prod) pp_env_stack env pp_env_stack new_env;
      inner new_env acc)
    acc productions
  in
  if debug then Lsp_io.log_debug "%a" pp_env_stack base_env;
  let comp_entries = inner base_env Expect.CompEntrySet.empty in
  if debug then (if Expect.CompEntrySet.cardinal comp_entries < 10 then Lsp_io.log_debug "=> Comp entries are [%a]\n" (Fmt.list ~sep:(Fmt.any ";") Expect.pp_completion_entry) (Expect.CompEntrySet.elements comp_entries) else Lsp_io.log_debug "=> Comp entries are %d\n" (Expect.CompEntrySet.cardinal comp_entries));
  comp_entries

let context_completion_items (doc:Lsp_document.t) Cobol_typeck.Outputs.{ group; _ } (pos:Position.t) =
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri doc.textdoc) in
  let range = range pos doc.textdoc in
  begin match Lsp_document.inspect_at ~position:(range.start) doc with
    | Some Env env ->
        map_completion_items ~range ~group ~filename (expected_tokens env)
    | _ -> [] end
