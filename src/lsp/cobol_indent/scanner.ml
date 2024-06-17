(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

type result = {
  toks : (Types.token * Types.token_descr) list ;
  revedits : Types.indent_record list (* in reverse order *) ;
  skipped_revlines : int list;
}

let verbose = Engine.verbose

(* all lines start:
   * if source_format.free, at position 0
   * if not source_format.free, at position skip_before+1
*)

let revtokens_of_line ~filename ~edit ~config line =
  if config.verbosity > 2 then
    Printf.eprintf "\n%!";
  let line = String.uppercase_ascii line in
  Lexer.init ();

  let rec iter tok_edit rev lexbuf =
    match Lexer.line lexbuf with
    | EOF -> rev
    | COMMENT _ ->
      iter tok_edit rev lexbuf
    | token ->
      if config.verbosity > 2 then
        Printf.eprintf "[[%s]]%!" (Lexing.lexeme lexbuf);

      let tok_indent = Lexing.lexeme_start lexbuf in
      let tok_length = Lexing.lexeme_end lexbuf - tok_indent in
      let tok = token, {
          tok_indent = tok_indent - config.whole_file_indent  ;
          tok_length ; tok_edit } in
      let next_edit = match tok_edit with
        | { bol = true ; edit } -> { bol = false ; edit }
        | _ -> tok_edit
      in
      iter next_edit ( tok :: rev ) lexbuf
    | exception exn ->
      let pos = lexbuf.Lexing.lex_curr_p.pos_cnum in
      Printf.eprintf "Error %s in %S, at position %d: %S\n%!"
        ( Printexc.to_string exn )
        filename
        pos
        ( String.sub line pos ( String.length line - pos) )
      ;
      exit 2
  in

  let lexbuf = Lexing.from_string line in
  let tokens = iter edit [] lexbuf  in
  tokens


let get_indent s =
  let rec iter s pos len =
    if pos = len then pos
    else
      match s.[pos] with
        ' ' | '\t' -> iter s (pos+1) len
      | _ -> pos
  in
  iter s 0 ( String.length s )

let tokens_of_lines ~filename ~config ~contents skipped_revlines lines =
  let revedits = ref [] in

  let base_indent =
    if config.source_format.free then
      0
    else
      1 + config.source_format.skip_before
  in

  let rec iter ~maybe_comment_entry rev lines =
    let has_continuation =
      match lines with
      | _ :: ( true, _, _, _ ) :: _ -> true
      | _ -> false
    in
    match lines with

    | (is_continuation, line_num, pos0, text_len) :: lines ->

        let s = String.sub contents pos0 text_len in
        let indent = get_indent s in
        if maybe_comment_entry && indent > 3 then begin
          if verbose then
            Printf.eprintf "%04d <<%s>> OUT\n%!" line_num s;
          iter rev ~maybe_comment_entry:true lines
        end else
          let offset_orig =
            if config.scan_for_indent then
              indent - config.whole_file_indent
            else
              base_indent
          in
          let edit = { lnum = line_num ;
                       offset_orig ;
                       offset_modif = offset_orig ; } in
          revedits := edit :: !revedits ;
          let dont_indent_line = is_continuation || has_continuation in
          let edit =  { bol = not dont_indent_line ; edit } in
          let revtokens = revtokens_of_line ~edit ~filename ~config s in
          let maybe_comment_entry =
            not config.source_format.free &&
            match revtokens with
            | (INFORMATION _, _) :: _ -> config.scan_for_indent
            | (DOT, _) :: (PROGRAM_ID, _) :: _ -> config.scan_for_indent
            | _ -> false
          in
          if verbose then
            Printf.eprintf "%04d ||%s||%s%s\n%!" line_num s
              (if dont_indent_line then " SKIPPED" else "")
              (if maybe_comment_entry then "BEGIN COMMENT ENTRY" else "")
          ;
          iter ~maybe_comment_entry ( revtokens @ rev ) lines
    | [] ->
        List.rev rev
  in

  let tokens = iter [] ~maybe_comment_entry:false lines in

  { toks = tokens ;
    revedits = !revedits ;
    skipped_revlines ;
  }

let tokenize ~config ~filename ~contents =

  let source_format = config.source_format in
  let len = String.length contents in
  (*
  Printf.eprintf "FILE SIZE: %d\n%!" len;
  Printf.eprintf "FORMAT: { free=%b, skip_before=%d, skip_after=%d }\n"
    source_format.free source_format.skip_before source_format.skip_after;
*)
  let lines = ref [] in
  let skipped_revlines = ref [] in

  let max_text_len =
    if source_format.free then
      65536
    else
      source_format.max_text_length
  in

  (* failwith "indent_contents not implemented" *)
  let rec iter pos line =
    (*  Printf.eprintf "line=%d\n%!" line; *)
    skip_before source_format.skip_before pos line

  and skip_before skip pos line =
    if pos = len then
      () (* we are done *)
    else
    if skip = 0 then
      iter_line pos line
    else
      let c = contents.[pos] in
      if c = '\n' then
        iter (pos+1) (line+1) (* empty line. TODO: remove useless spaces *)
      else begin
        (*
        Printf.eprintf "c='%c' skip=%d skip=%d\n%!"
          c skip source_format.skip_before ;
*)
        if c = '\t'
        && not source_format.free
        && skip = 6
        && source_format.skip_before = 6 then
          iter_in_line false pos (pos+1) line
        else
          skip_before (skip-1) (pos+1) line
      end

  and iter_line pos line =
    (* We are at the beginning of line. We have to deal with
       the indicator in fixed-format.
    *)
    (*    Printf.eprintf "iter_line\n"; *)
    let c = contents.[pos] in
    if c = '\n' then
      iter (pos+1) (line+1)
    else
    if source_format.free then
      iter_in_line false pos (pos+1) line
    else
      match c with
      | '-' -> iter_in_line true (pos+1) (pos+1) line
      | '*' -> skip_line pos (pos+1) line
      | ' ' -> iter_in_line false (pos+1) (pos+1) line
      | _ ->
        if not config.scan_for_indent then
          skipped_revlines := line :: !skipped_revlines ;
        skip_line pos (pos+1) line

  and iter_in_line cont pos0 pos line =
    (*    Printf.eprintf "iter_in_line\n"; *)
    if pos = len then
      add_line cont pos0 pos line
    else
      let c = contents.[pos] in
      if c = '\n' then begin
        add_line cont pos0 pos line;
        iter (pos+1) (line+1)
      end
      else
        iter_in_line cont pos0 (pos+1) line

  and add_line cont pos0 pos line =
    if pos > pos0 then
      let pos =
        if contents.[pos-1] = '\r' then pos-1 else pos
      in
      let text_len = min (pos - pos0) max_text_len in
      lines := ( cont, line, pos0, text_len ) :: !lines

  and skip_line pos0 pos line =
    if pos < len then
      let c = contents.[pos] in
      if c = '\n' then begin
        if verbose then
          Printf.eprintf "%04d %S(comment)\n%!" line
        (String.sub contents pos0 (pos-pos0));
        iter (pos+1) (line+1)
      end else
        skip_line pos0 (pos+1) line

  in
  iter 0 1;
  let lines = List.rev !lines in

  tokens_of_lines ~filename ~config ~contents !skipped_revlines lines
