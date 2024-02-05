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

open Ez_file.V1

open Types

let verbose = Engine.verbose

(* This function applies the ~edits to the file ~filename *)

let apply_edits ~contents ~range ~config ~filename ~edits =

  let len = String.length contents in
  let b = Buffer.create ( 2 * len ) in
  let add_char pos =
    Buffer.add_char b contents.[pos]
  in

  let rec iter_edits ~pos ~line edits =
    if verbose then
      Printf.eprintf "iter_edits ~pos:%d ~line:%d\n%!" pos line;
    match edits with
    | [] -> Buffer.add_string b ( String.sub contents pos (len-pos) )
    | edit :: edits ->

      if edit.lnum < range.start_line || edit.lnum > range.end_line then
        iter_edits ~pos ~line edits
      else
        iter_edit ~pos edit ~line edits

  and iter_edit ~pos edit ~line edits =
    if verbose then
      Printf.eprintf "iter_edit ~pos:%d ~line:%d ~edit:%d\n%!" pos line
        edit.lnum;
    if line = edit.lnum then
      skip_before ~pos
        ~nbefore:(
          (if config.source_format.free then 0 else 1) +
          config.source_format.skip_before )
        edit
        ~line edits
    else
      skip_line ~pos edit ~line edits

  and skip_before ~pos ~nbefore edit ~line edits =
    if verbose then
      Printf.eprintf "skip_before ~pos:%d ~line:%d ~nbefore:%d\n%!"
        pos line nbefore;
    if nbefore > 0 then
      let c = contents.[pos] in
      assert ( c <> '\n' && c <> '\r' );
      if c = '\t' then

        let pos =
          if edit.offset_orig > edit.offset_modif then
            (* TAB is 8, i.e. more than the indent to area A. For now,
               we always expect TAB to be at the first position of the
               line, and consider other TABs as simple chars *)
            let pos = pos+1 in
            let spaces_to_remove = edit.offset_orig - edit.offset_modif in

            if spaces_to_remove <= 8 then begin
              for _ = 1 to 8-spaces_to_remove do
                Buffer.add_char b ' ';
              done;
              pos
            end else
              let error = true in
              let nspaces = spaces_to_remove-8 in
              remove_spaces ~error ~pos ~nspaces ~line
          else begin
            add_char pos ;
            let pos = pos + 1 in
            let nspaces = edit.offset_modif - edit.offset_orig in
            add_spaces ~pos ~nspaces ~line
          end
        in
        let addspaces = edit.offset_modif - edit.offset_orig in
        iter_eol ~pos ~addspaces ~line edits
      else begin
        add_char pos ;
        skip_before ~pos:(pos+1) ~nbefore:(nbefore-1) edit ~line edits
      end
    else
      let pos =
        if edit.offset_orig > edit.offset_modif then
          let error = true in
          remove_spaces ~pos ~line ~error
            ~nspaces:(edit.offset_orig - edit.offset_modif)
        else
          add_spaces ~pos ~line
            ~nspaces:(edit.offset_modif - edit.offset_orig)
      in
      let addspaces = edit.offset_modif - edit.offset_orig in
      iter_eol ~pos ~addspaces ~line edits

  (* We have corrected the indentation by ~addspaces:
     * if addspaces>0, if we reach a textlen of ~maxtextlen, we MUST
       remove at least addspaces spaces at the end.
     * if addspaces<0, if we reach a textlen of ~maxtextlen-addspaces,
       we MUST add -addspaces spaces at the end *)

  and iter_eol ~pos ~addspaces ~line edits =
    if verbose then
      Printf.eprintf "iter_eol ~pos:%d ~line:%d ~addspaces:%d\n%!"
        pos line addspaces;
    if config.source_format.free then
      iter_edits ~pos ~line edits
    else
      let textlen =
        if addspaces > 0 then
          addspaces
        else
          -addspaces
      in
      iter_eol_step ~pos ~textlen ~addspaces ~line edits

  and iter_eol_step ~pos ~textlen ~addspaces ~line edits =
    if verbose then
      Printf.eprintf
        "iter_eol_step ~pos:%d ~line:%d ~textlen:%d ~addspaces:%d\n%!"
        pos line textlen addspaces;
    if textlen = config.source_format.max_text_length then
      let pos =
        if addspaces > 0 then
          remove_spaces ~error:false ~pos ~nspaces:addspaces ~line
        else
          add_spaces ~pos ~nspaces:(-addspaces) ~line
      in
      iter_edits ~pos ~line edits
    else
    if pos = len then
      iter_edits ~pos ~line edits
    else
      let c = contents.[pos] in
      match c with
      | '\n' ->
        iter_edits ~pos ~line edits
      | _ ->
        add_char pos;
        let pos = pos + 1 in
        let textlen = textlen+1 in
        iter_eol_step ~pos ~textlen ~addspaces ~line edits

  and add_spaces ~pos ~nspaces ~line =
    if verbose then
      Printf.eprintf "add_spaces ~pos:%d ~line:%d\n%!" pos line;
    Buffer.add_string b ( String.make nspaces ' ' );
    pos

  and remove_spaces ~error ~pos ~nspaces ~line =
    if verbose then
      Printf.eprintf "remove_spaces ~pos:%d ~line:%d\n%!" pos line;
    if nspaces > 0 then begin
      match contents.[pos] with
      | ' ' | '\t' ->
        remove_spaces ~error ~pos:(pos+1) ~nspaces:(nspaces-1) ~line
      | _ ->
        if error then begin
          Printf.eprintf "Error: pos=%d line=%d nspaces=%d char=%S\n%!"
            pos line nspaces
            ( String.make 1 ( contents.[pos] ));
          exit 2
        end;
        pos
    end else
      pos

  and skip_line ~pos edit ~line edits =
    if verbose then
      Printf.eprintf "skip_line ~pos:%d ~line:%d\n%!" pos line;
    add_char pos ;
    if contents.[pos] = '\n' then
      iter_edit ~pos:(pos+1) edit ~line:(line+1) edits
    else
      skip_line ~pos:(pos+1) edit ~line edits


  in
  iter_edits ~pos:0 ~line:1 edits; (* lnum starts at 1 ? *)
  let contents = Buffer.contents b in
  if filename = "-" then
    Printf.printf "%s%!" contents
  else begin
    EzFile.write_file filename contents ;
    Printf.eprintf "File %S indented\n%!" filename
  end
