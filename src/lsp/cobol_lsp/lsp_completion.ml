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

open Cobol_common                                                  (* Visitor *)
open Cobol_common.Srcloc.INFIX

open Lsp_completion_keywords
open Lsp.Types

let name_proposals ast ~filename pos =
  let visitor = object
    inherit [StringSet.t] Cobol_ptree.Visitor.folder

    method! fold_compilation_unit' cu =
      if Lsp_position.is_in_srcloc ~filename pos ~@cu
      then Visitor.do_children
      else Visitor.skip_children

    method! fold_name name acc =
      Visitor.skip_children @@ StringSet.add name acc

    end
  in
  Cobol_ptree.Visitor.fold_compilation_group visitor ast StringSet.empty
  |> StringSet.elements

(*If need be, get the qualname_proposals "X OF Y"... from the definition maps*)


(*TODO: If the partial parsing could give more information
        like in which statement the position is(or even better, in which clause/phrase),
        Then we can remove the keywords that cannot appear in this statement from
        the keyword list.
*)
(* type div =
| Ident_div
| Env_div
| Data_div
| Proc_div

let keyword_proposals ast pos =
  let visitor = object
    inherit [div option] Cobol_parser.PTree_visitor.folder

    method! fold_data_division' {loc; _} _ =
      Visitor.skip_children @@
        if Lsp_position.is_in_srcloc pos loc
        then Some Data_div
        else None

    method! fold_procedure_division' {loc; _} _ =
      Visitor.skip_children @@
        if Lsp_position.is_in_srcloc pos loc
        then Some Proc_div
        else None

    end
  in
  match Cobol_parser.PTree_visitor.fold_compilation_group visitor ast None with
  | Some Proc_div -> keywords_proc
  | Some Data_div -> keywords_data (*does not work*)
  | _ -> [] *)

let keyword_proposals _ast _pos = keywords_all

let completion_items text (pos:Position.t) ast =
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri text) in
  let range =
    let line = pos.line in
    let character = pos.character in
    let texts = String.split_on_char '\n' @@ Lsp.Text_document.text text in
    let text_line = List.nth texts line in
    let index = 1 + String.rindex_from text_line (character - 1) ' ' in
    let position_start = Position.create ~character:index ~line in
    Range.create ~start:position_start ~end_:pos
  in

  let names = name_proposals ast ~filename pos in
  let keywords = keyword_proposals ast pos in
  let words = names @ keywords in

  List.map (fun x ->
    let textedit = TextEdit.create ~newText:x ~range in
    (*we may change the ~sortText/preselect for reason of priority *)
    CompletionItem.create
      ~label:x
      ~sortText:x
      ~preselect:false
      ~textEdit:(`TextEdit textedit)
      ())
    words
