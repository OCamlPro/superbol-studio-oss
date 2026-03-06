(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let expand_tabs ?(tab_stop=8) ?(starting_col=0) src =
  match String.index_opt src '\t' with
  | None -> src
  | Some _ ->
      let buf = Buffer.create (String.length src) in
      let col = ref starting_col in
      let spaces = String.make tab_stop ' ' in
      String.iter (function
        | '\t' ->
            let n = (tab_stop - !col mod tab_stop) in
            Buffer.add_substring buf spaces 0 n;
            col := !col + n
        | ('\n' | '\r') as c ->
            Buffer.add_char buf c;
            col := 0;
        | c ->
            Buffer.add_char buf c;
            incr col)
      src;
      Buffer.contents buf

let from_channel_expanding_tabs ?with_positions ?(tab_stop = 8) ic : Lexing.lexbuf =
    let read_buf = Bytes.create 4096 in
    let read_pos = ref 0 in    (* current position in read_buf *)
    let read_len = ref 0 in    (* valid bytes in read_buf *)
    let col = ref 0 in         (* current column (0-based) *)
    let refill buf len =
      let written = ref 0 in
      let rec loop () =
        if !written >= len then ()       (* lexer buffer full *)
        else begin
          (* Refill read_buf if exhausted *)
          if !read_pos >= !read_len then begin
            let n = input ic read_buf 0 (Bytes.length read_buf) in
            if n = 0 then ()             (* EOF *)
            else begin read_pos := 0; read_len := n; loop () end
          end else
            let c = Bytes.get read_buf !read_pos in
            if c = '\t' then begin
              let spaces = tab_stop - (!col mod tab_stop) in
              let n = min spaces (len - !written) in
              Bytes.fill buf !written n ' ';
              written := !written + n;
              col := !col + n;
              if n = spaces then           (* fully expanded this tab *)
                incr read_pos
              (* else: partially expanded; we'll resume next refill *)
            end else begin
              Bytes.set buf !written c;
              incr written;
              incr read_pos;
              if c = '\n' then col := 0
              else col := !col + 1
            end;
            loop ()
        end
      in
      loop ();
      !written
    in
    Lexing.from_function ?with_positions refill


