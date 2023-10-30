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


(** [apply s rdl] applies the changes in [rdl] to the text in [s]. [rdl] is
    expected to have been computed by calling [indent_range ~contents:s].

    Note: this function is currently untested. It is only used by the command
    `superbol indent`. The main way to interact with the indenter is through LSP
    requests, which are tested in lsp_formatting.ml *)
let apply : string -> indent_record list -> string =
  (*
    [irs] contains a list of {!indent_record} entries describing how to
     modify the indentation.  They must be sorted in increasing order, and there
     must be at most one record per line.

    [buf] is a {!Buffer.t} into which we are writing the indented content.

    [s] is the original content to indent as a {!string}.

    [line] is the current line, which starts at offset [bol] in the string [s].

    [start] is an offset in [s], less than or equal to [bol], that must be
    preserved as-is. This allows copying large unmodified chunks of text (lines
    that are already properly indented) using a single call to
    {!Buffer.add_substring} for reduced overhead.
  *)
  let rec aux ~start (irs : indent_record list) buf s line bol =
    (* Note: this can raise [Invalid_argument] if the modifications do not
       cleanly apply to the input string, which include cases where there are
       modifications on lines past the end of the input, modifications that are
       not sorted, etc. -- we can call [invalid_arg] ourselves, but
       [String.index_from] can also raise [Invalid_argument] in these
       conditions. *)
    match irs with
    | [] when start = 0 -> s
    | [] ->
      Buffer.add_substring buf s start (String.length s - start);
      Buffer.contents buf
    | { lnum; offset_orig; offset_modif } :: irs when line = lnum - 1 ->
      let eol =
        try String.index_from s bol '\n' + 1 with Not_found -> String.length s
      in
      let delta = offset_modif - offset_orig in
      if delta > 0 then (
        Buffer.add_substring buf s start (bol + offset_orig - start);
        for _ = 0 to delta - 1 do
          Buffer.add_char buf ' '
        done;
      ) else (
        Buffer.add_substring buf s start (bol - start + offset_orig + delta);
      );
      aux ~start:(bol + offset_orig) irs buf s (line + 1) eol
    | _ :: _ ->
      match String.index_from s bol '\n' + 1 with
      | eol -> aux ~start irs buf s (line + 1) eol
      | exception Not_found ->
        (* Modifications on lines past the end of the input. *)
        invalid_arg "Cobol_indent.apply"
  in
  fun s rdl ->
    try
      let buf = Buffer.create (String.length s) in
      aux ~start:0 (List.rev rdl) buf s 0 0
    with Invalid_argument _ ->
      Fmt.failwith "Could not indent: internal error"
