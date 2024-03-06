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

open Types

type indicator =
  | COMMENT
  | LINE
  | CONTINUATION

type state =
  | INITIAL
  | NORMAL
  | LITERAL_OPENED of char
  | LITERAL_CLOSED_ON_BORDER of char

exception Error of int * string
let error pos fmt =
  Printf.ksprintf (fun s -> raise (Error (pos, s))) fmt

let parse_text ?(cont=INITIAL) ~source_format text =
  let len = String.length text in
  let init_pos = ref 0 in
  let rec iter_space pos0 pos =
    if pos = len then
      NORMAL, String.sub text !init_pos (pos0- !init_pos)
    else
      let c = text.[pos] in
      match c with
      | ' ' ->
        iter_space pos0 (pos+1)
      | '"' ->
        iter_literal c (pos+1)
      | _ ->
        iter_non_space (pos+1)

  and iter_non_space pos =
    if pos = len then
      NORMAL, String.sub text !init_pos (pos- !init_pos)
    else
      let c = text.[pos] in
      match c with
      | ' ' ->
        iter_space pos (pos+1)
      | '"' ->
        iter_literal c (pos+1)
      | _ ->
        iter_non_space (pos+1)

  and iter_literal delim pos =
    if pos = len then
      LITERAL_OPENED delim, String.sub text !init_pos (pos- !init_pos)
    else
      let c = text.[pos] in
      if c = delim then
        let pos=pos+1 in
        if pos = source_format.max_text_length then begin
          LITERAL_CLOSED_ON_BORDER delim,
          String.sub text !init_pos (pos- !init_pos)
        end else
          iter_space pos pos
      else
        iter_literal delim (pos+1)

  and iter_cont pos =
    if pos = len then
      NORMAL, ""
    else
      let c = text.[pos] in
      match c with
      | ' ' -> iter_cont (pos+1)
      | _ ->
        init_pos := pos;
        iter_non_space pos

  and iter_cont_literal double delim pos =
    if pos = len then
      let state =
        if double then
          LITERAL_CLOSED_ON_BORDER delim
        else
          LITERAL_OPENED delim
      in
      state, ""
    else
      let c = text.[pos] in
      match c with
      | ' ' -> iter_cont_literal double delim (pos+1)
      | _ ->
        if c = delim then
          let pos = pos+1 in
          if double then
            if pos = len || text.[pos] <> delim then
              failwith "expecting double literal delimiter for continuation"
            else
              let pos = pos+1 in
              init_pos := pos;
              iter_literal delim  pos
          else begin
            init_pos := pos;
            iter_literal delim  pos
          end
        else
          failwith "expecting literal delimiter for continuation"
  in
  match cont with
  | INITIAL ->
    iter_space 0 0
  | NORMAL ->
    iter_cont 0
  | LITERAL_OPENED delim ->
    iter_cont_literal false delim 0
  | LITERAL_CLOSED_ON_BORDER delim ->
    iter_cont_literal true delim 0

(* convert a file content from some fixed format to free format *)
let to_free ~source_format contents =

  let source_format = Config.source_format source_format in

  let len = String.length contents in
  let b = Buffer.create len in

  let output fmt =
    Printf.bprintf b fmt
  in

  let current_state = ref INITIAL in
  let output_newline () =
    match !current_state with
    | INITIAL -> ()
    | _ -> output "\n"
  in
  let new_line pos kind text =
    match kind with
    | COMMENT ->
      output_newline ();
      output "*> %s\n" text ;
      current_state := INITIAL (* newline has been sent *)
    | LINE ->
      output_newline ();
      let state, text = parse_text ~source_format text in
      output "%s" text;
      current_state := state
    | CONTINUATION ->
      match !current_state with
      | INITIAL ->
        error pos "line cannot continue previous line"
      | _ ->
        let state, text =
          try
            parse_text ~cont:!current_state ~source_format text
          with
          | Failure msg ->
            error pos "%s" msg
        in
        output "%s" text;
        current_state := state
  in

  let new_line pos0  pos =
    let len = pos - pos0 in
    (* Printf.eprintf "new_line %d %d [%d]\n%!" pos0 pos len ; *)
    if len > source_format.skip_before then
      let indicator = contents.[pos0+source_format.skip_before] in
      let text =
        let text_len = len - source_format.skip_before - 1 in
        let text_len = min source_format.max_text_length text_len in
        String.sub contents (pos0+source_format.skip_before+1) text_len
      in
      match indicator with
      | '*' | '/' ->
        new_line pos0 COMMENT text
      | ' ' ->
        new_line pos0 LINE text
      | '-' ->
        new_line pos0 CONTINUATION text
      | 'd' | 'D' ->
        new_line pos0 COMMENT (" >>Debug: " ^ text)
      | _ ->
        error pos "unknown indicator '%c'\n%!" indicator
    else
      new_line pos0 LINE ""
  in

  let rec iter pos0 pos =
    if pos = len then
      new_line pos0 pos
    else
      let c = contents.[pos] in
      if c = '\r' then begin
        new_line pos0 pos;
        let pos = pos+1 in
        if pos = len then
          ()
        else
        if contents.[pos] = '\n' then
          let pos = pos+1 in
          iter pos pos
        else
          error pos "carriage-return without newline"
      end else
      if c = '\n' then begin
        new_line pos0 pos;
        let pos = pos+1 in
        iter pos pos
      end
      else
        let pos = pos+1 in
        iter pos0 pos
  in
  iter 0 0;

  Buffer.contents b
