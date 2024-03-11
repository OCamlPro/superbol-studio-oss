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

(* ~char: the position within the line. For now, it does not change with
   insertions/deletions, but it should, in the future... *)

let apply_edits ~contents ~range ~config ~filename ~edits ~symbolic =

  let len = String.length contents in
  let b = Buffer.create (if symbolic then 4 else 2 * len ) in
  let ops = ref [] in

  let add_op ~line ~char spaces =
    if spaces <> 0 then
      ops := { line ; char ; spaces } :: !ops
  in
  let add_char pos =
    if not symbolic then Buffer.add_char b contents.[pos]
  in

  let rec iter_edits ~pos ~char ~line edits =
    if verbose then
      Printf.eprintf "iter_edits ~pos:%d ~char:%d ~line:%d\n%!" pos char line;
    match edits with
    | [] ->
      if not symbolic then
        Buffer.add_string b ( String.sub contents pos (len-pos) )
    | edit :: edits ->

      if edit.lnum < range.start_line || edit.lnum > range.end_line then
        iter_edits ~pos ~char ~line edits
      else
        iter_edit ~pos ~char edit ~line edits

  and iter_edit ~pos ~char edit ~line edits =
    if verbose then
      Printf.eprintf "iter_edit ~pos:%d ~line:%d ~edit:%d\n%!" pos line
        edit.lnum;
    if line = edit.lnum then
      skip_before ~pos ~char
        ~nbefore:(
          (if config.source_format.free then 0 else 1) +
          config.source_format.skip_before )
        edit
        ~line edits
    else
      skip_line ~pos ~char edit ~line edits

  and skip_before ~pos ~char ~nbefore edit ~line edits =
    if verbose then
      Printf.eprintf "skip_before ~pos:%d ~line:%d ~nbefore:%d\n%!"
        pos line nbefore;
    if nbefore > 0 then
      let c = contents.[pos] in
      if c = '\n' || c = '\r' then
        Printf.kprintf failwith
          "Cobol_indent.Editor.skip_before: char \\%3d at pos %d, line %d, skipping %d" (int_of_char c) pos line nbefore ;

      if c = '\t' then

        let pos, char =
          if edit.offset_orig > edit.offset_modif then
            (* TAB is 8, i.e. more than the indent to area A. For now,
               we always expect TAB to be at the first position of the
               line, and consider other TABs as simple chars *)
            let pos = pos+1 in
            add_op ~line ~char (-1);
            let char = char+1 in
            let spaces_to_remove = edit.offset_orig - edit.offset_modif in

            if spaces_to_remove <= 8 then begin
              let spaces = 8-spaces_to_remove in
              add_op ~line ~char spaces;
              if not symbolic then
                for _ = 1 to spaces do
                  Buffer.add_char b ' ';
                done;
              pos, char
            end else
              let error = true in
              let nspaces = spaces_to_remove-8 in
              remove_spaces ~error ~pos ~char ~nspaces ~line
          else begin
            add_char pos ;
            let pos = pos+1 in
            let char = char+1 in
            let nspaces = edit.offset_modif - edit.offset_orig in
            add_spaces ~pos ~char ~nspaces ~line
          end
        in
        let addspaces = edit.offset_modif - edit.offset_orig in
        iter_eol ~pos ~char ~addspaces ~line edits
      else begin
        add_char pos ;
        let pos = pos+1 in
        let char = char+1 in
        skip_before ~pos ~char ~nbefore:(nbefore-1) edit ~line edits
      end
    else
      let pos, char =
        if edit.offset_orig > edit.offset_modif then
          let error = true in
          remove_spaces ~pos ~char ~line ~error
            ~nspaces:(edit.offset_orig - edit.offset_modif)
        else
          add_spaces ~pos ~char ~line
            ~nspaces:(edit.offset_modif - edit.offset_orig)
      in
      let addspaces = edit.offset_modif - edit.offset_orig in
      iter_eol ~pos ~char ~addspaces ~line edits

  (* We have corrected the indentation by ~addspaces:
     * if addspaces>0, if we reach a textlen of ~maxtextlen, we MUST
       remove at least addspaces spaces at the end.
     * if addspaces<0, if we reach a textlen of ~maxtextlen-addspaces,
       we MUST add -addspaces spaces at the end *)

  and iter_eol ~pos ~char ~addspaces ~line edits =
    if verbose then
      Printf.eprintf "iter_eol ~pos:%d ~line:%d ~addspaces:%d\n%!"
        pos line addspaces;
    if config.source_format.free then
      iter_edits ~pos ~char ~line edits
    else
      let textlen =
        if addspaces > 0 then
          addspaces
        else
          -addspaces
      in
      iter_eol_step ~pos ~char ~textlen ~addspaces ~line edits

  and iter_eol_step ~pos ~char ~textlen ~addspaces ~line edits =
    if verbose then
      Printf.eprintf
        "iter_eol_step ~pos:%d ~line:%d ~textlen:%d ~addspaces:%d\n%!"
        pos line textlen addspaces;
    if textlen = config.source_format.max_text_length then
      let pos, char =
        if addspaces > 0 then
          remove_spaces ~error:false ~pos ~char ~nspaces:addspaces ~line
        else
          add_spaces ~pos ~char ~nspaces:(-addspaces) ~line
      in
      iter_edits ~pos ~char ~line edits
    else
    if pos = len then
      iter_edits ~pos ~char ~line edits
    else
      let c = contents.[pos] in
      match c with
      | '\n' ->
        iter_edits ~pos ~char ~line edits
      | _ ->
        add_char pos;
        let pos = pos+1 in
        let char = char+1 in
        let textlen = textlen+1 in
        iter_eol_step ~pos ~char ~textlen ~addspaces ~line edits

  and add_spaces ~pos ~char ~nspaces ~line =
    if verbose then
      Printf.eprintf "add_spaces ~pos:%d ~line:%d\n%!" pos line;
    add_op ~char ~line nspaces ;
    if not symbolic then
      Buffer.add_string b ( String.make nspaces ' ' );
    pos, char

  and remove_spaces ~error ~pos ~char ~nspaces ~line =
    if verbose then
      Printf.eprintf "remove_spaces ~pos:%d ~line:%d\n%!" pos line;
    add_op ~line ~char (-nspaces);
    remove_spaces_step ~error ~pos ~char ~nspaces ~line

  and remove_spaces_step ~error ~pos ~char ~nspaces ~line =
    if verbose then
      Printf.eprintf "remove_spaces_step ~pos:%d ~line:%d\n%!" pos line;
    if nspaces > 0 then begin
      match contents.[pos] with
      | ' ' | '\t' ->
        let pos = pos+1 in
        let char = char+1 in
        remove_spaces_step ~error ~pos ~char ~nspaces:(nspaces-1) ~line
      | _ ->
        if error then begin
          Printf.eprintf "Error: pos=%d line=%d nspaces=%d char=%S\n%!"
            pos line nspaces
            ( String.make 1 ( contents.[pos] ));
          exit 2
        end;
        pos, char
    end else
      pos, char

  and skip_line ~pos ~char edit ~line edits =
    if verbose then
      Printf.eprintf "skip_line ~pos:%d ~char:%d ~line:%d\n%!" pos char line;
    add_char pos ;
    let c = contents.[pos] in
    let pos = pos+1 in
    if c = '\n' then
      let char = 0 in
      iter_edit ~pos ~char edit ~line:(line+1) edits
    else
      let char = char+1 in
      skip_line ~pos ~char edit ~line edits

  in
  iter_edits ~pos:0 ~char:0 ~line:1 edits; (* lnum starts at 1 ? *)
  if not symbolic then begin
    let contents = Buffer.contents b in
    if filename = "-" then
      Printf.printf "%s%!" contents
    else begin
      EzFile.write_file filename contents ;
      Printf.eprintf "File %S indented\n%!" filename
    end;
  end;
  List.rev !ops
