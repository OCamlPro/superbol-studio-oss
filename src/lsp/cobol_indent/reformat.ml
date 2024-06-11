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

type context = {
  mutable current_state : state ;
  buffer : Buffer.t ;
  mutable source_format : source_format ;
  target_format : source_format ;
}

exception Error of int * string
let error pos fmt =
  Printf.ksprintf (fun s -> raise (Error (pos, s))) fmt

let output context fmt =
  Printf.bprintf context.buffer fmt


let output_newline context =
  match context.current_state with
  | INITIAL -> ()
  | _ -> output context "\n"

module FIXED2FREE = struct

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

  let new_line context pos kind text =
    let source_format = context.source_format in
    match kind with
    | COMMENT ->
      output_newline context;
      output context "*> %s\n" text ;
      context.current_state <- INITIAL (* newline has been sent *)
    | LINE ->
      output_newline context;
      let state, text = parse_text ~source_format text in
      output context "%s" text;
      context.current_state <- state
    | CONTINUATION ->
      match context.current_state with
      | INITIAL ->
        error pos "line cannot continue previous line"
      | _ ->
        let state, text =
          try
            parse_text ~cont:context.current_state ~source_format text
          with
          | Failure msg ->
            error pos "%s" msg
        in
        output context "%s" text;
        context.current_state <- state

end

let is_source_pragam context pos text =
  match EzString.split_simplify ( String.uppercase_ascii text ) ' ' with
  | ">>SOURCE" :: tokens
  | ">>" :: "SOURCE" :: tokens ->
    let tokens =
      match tokens with
      | "FORMAT" :: tokens
      | tokens -> tokens
    in
    let tokens =
      match tokens with
      | "IS" :: tokens
      | tokens -> tokens
    in
    context.source_format <-
      begin
        match tokens with
        | [ "FIXED" ] -> Config.fixed_format
        | [ "FREE" ] -> Config.free_format
        | [ "COBOLX" ] -> Config.cobolx_format
        | [ "VARIABLE" ] -> Config.variable_format
        | [ "XCARD" ] -> Config.xcard_format
        | _ ->
          error pos "wrong >>SOURCE line"
      end;
    true
  | "$SET" :: tokens
  | "$" :: "SET" :: tokens
  | ">>SET" :: tokens
  | ">>" :: "SET" :: tokens
    ->
    begin
      let set_format format =
        context.source_format <-
          begin
            match format with
            | "\"FIXED\"" -> Config.fixed_format
            | "\"FREE\"" -> Config.free_format
            | "\"COBOLX\"" -> Config.cobolx_format
            | "\"VARIABLE\"" -> Config.variable_format
            | "\"XCARD\"" -> Config.xcard_format
            | _ ->
              error pos "wrong >>SOURCE line"
          end;
        true

      in
      match tokens with
      | [ "SOURCEFORMAT" ; format ] ->
        set_format format
      | [ token ] ->
        begin
          match EzString.chop_prefix token ~prefix:"SOURCEFORMAT" with
          | None -> false
          | Some format ->
            set_format format
        end
      | _ -> false
    end
  | _ -> false

(* convert a file content from some fixed format to free format *)
let to_free ~source_format ~filename ~contents =

  let len = String.length contents in
  let buffer = Buffer.create len in

  let target_format = Config.free_format in

  let context = {
    buffer ;
    current_state = INITIAL ;
    source_format = Config.source_format source_format ;
    target_format ;
  }
  in

  let new_line pos kind text =
    match context.source_format.format, context.target_format.format with
    | SFFixed, SFFree ->
      FIXED2FREE.new_line context pos kind text
    | SFFree, SFFree ->
      output_newline context ;
      context.current_state <- INITIAL ;
      output context "%s\n" text
    | _ -> assert false
  in

  let new_line pos0  pos =
    let len = pos - pos0 in
    (* Printf.eprintf "new_line %d %d [%d]\n%!" pos0 pos len ; *)
    if len > context.source_format.skip_before then
      if context.source_format.format = SFFree then begin
        (* This is more complicated,
           as we must take care of inline comments... *)
        let text = String.sub contents pos0 len in
        if not ( is_source_pragam context pos0 text ) then
          new_line pos0 LINE text
      end
      else
        let indicator = contents.[pos0+context.source_format.skip_before] in
        let text =
          let text_len = len - context.source_format.skip_before - 1 in
          let text_len = min context.source_format.max_text_length text_len in
          String.sub contents (pos0+context.source_format.skip_before+1) text_len
        in
        match indicator with
        | '*' | '/' ->
          new_line pos0 COMMENT text
        | ' ' ->
          if not ( is_source_pragam context pos0 text ) then
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

  let contents =
    Buffer.contents buffer
  in
  Main.indent
    ~source_format: (SF target_format.format)
    ~filename
    ~contents
    Output_contents
