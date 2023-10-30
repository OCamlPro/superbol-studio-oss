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

open Cobol_common.Srcloc

open Indent_type
open Indent_keywords

let check_pos
  (pos:Lexing.position)
  (offset:int)
  (ind_recds:indent_record list)
  ~print_errors
=
  let real_offset = pos.pos_cnum - pos.pos_bol in
  if real_offset <> offset then
    begin
    if print_errors then
      begin
        let newsrcloc = raw (pos,pos) in
        Fmt.pr "%a" Cobol_common.Srcloc.pp_srcloc newsrcloc;
        Fmt.pr "Indentation error : the offset is %d, but expected offset is %d\n\n" real_offset offset
      end;
    {lnum = pos.pos_lnum;
     offset_orig = real_offset;
     offset_modif = offset}
    :: ind_recds
    end
  else
    ind_recds

(* print_errors for debug *)
let check_pos = check_pos ~print_errors:false

let check_pos src_format srcloc offset ind_recds ifcheck =
  match src_format with
  | Cobol_preproc.Src_format.SF (NoIndic, FreePaging) when ifcheck ->
    let pos = start_pos srcloc in
    check_pos pos offset ind_recds
  | _ ->
    (* Indenting temporarily disabled in fixed format
        https://github.com/OCamlPro/superbol-studio-oss/issues/52

        Support must be improved before enabling again, in particular to
        avoid pushing content into the margin.
        https://github.com/OCamlPro/superbol-studio-oss/issues/45
      *)
    ind_recds

let failure_msg loc =
  let pos = start_pos loc in
  let lnum, cnum = pos.pos_lnum, pos.pos_cnum - pos.pos_bol in
  "line: "^ (string_of_int lnum) ^" character: "^ (string_of_int cnum)

let offset_of_keyword = Indent_config.offset_of_keyword

let offset_of_context context =
  match context with
  | [] -> failwith "empty context"
  | _ -> snd @@ List.hd context

let push_context key context =
  (key, offset_of_keyword key + offset_of_context context) :: context


(*for DATA DECLARATION*)
let is_data_decl (str:string) =
  match Int32.of_string_opt str with
  | None -> false
  | _ -> true

let rec reduce_level i context =
  match context with
  | (LEVEL j, _) :: context
    when j >= i ->
      reduce_level i context
  | _ ->
    context

(*used for "." *)
let rec handle_period context =
  match context with
  | []
  | ((BEGIN | COMPILATION_UNIT
      | IDENT_DIV | ENV_DIV| DATA_DIV | PROC_DIV
      | DECLARATIVES | LEVEL _| PARAGRAPH | SECTION), _) :: _ ->
      context
  | _ :: context ->
      handle_period context

(* explicit scope termination*)
(* for END DECLARATIVES./END-XXX/ELSE/END PROGRAM/...*)
(* when using this function, [keyword] is always in [context] *)
let rec exp_scope_termination (keyword:context_kind) context =
  match context with
  | (x, _) :: context when x <> keyword ->
      exp_scope_termination keyword context
  | _ ->
      context

(* similar to exp_scope_termination, but we allow that [keyword] is not in [context]*)
(* must be careful to use [pop_until] and [exp_scope_termination].
   since we may indent a range of code, we cannot hope that the previous layer of
   [keyword] is already inside [context]:
   ex. maybe we indent a range of code which is a paragraph of procedure division,
       then the `SECTION` `PROC_DIV` is not in the initial context
      (but if we indent the whole file, `PROC_DIV` are always before `PARAGRAPH`)   *)
let rec pop_until (keyword:context_kind) context =
  match context with
  | (( BEGIN | COMPILATION_UNIT | IDENT_DIV | DATA_DIV
      | ENV_DIV | PROC_DIV | DECLARATIVES), _) :: _ ->
      context
  | (x, _) :: context when x <> keyword ->
      pop_until keyword context
  | _ ->
      context

(*pop the scope until the scope of division*)
let rec pop_until_division context =
  match context with
  | [] | ( (BEGIN|COMPILATION_UNIT|IDENT_DIV|DATA_DIV|ENV_DIV|PROC_DIV), _) :: _ -> context
  | _ :: context ->
      pop_until_division context


(*pop the scope until the scope of compilation_unit*)
let rec pop_until_compilation_unit context =
  match context with
  | [] | ( (BEGIN|COMPILATION_UNIT), _) :: _ -> context
  | _ :: context -> pop_until_compilation_unit context


(*implicit scope termination*)
let rec imp_scope_termination context =
  match context with
  | (keyword, _) :: context
    when not @@ is_not_imp_terminable keyword ->
      imp_scope_termination context
  | _ -> context

(*terminate phrase inside statement*)
let rec phrase_termination context =
  match context with
  | (key, _) :: context when is_phrase key ->
      phrase_termination context
  | _ -> context

let rec phrase_termination_until keyword context =
  match context with
  | (key, _) :: context
    when is_phrase key && key <> keyword ->
      phrase_termination_until keyword context
  | _ -> context
