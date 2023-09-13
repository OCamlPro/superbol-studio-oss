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

open Cobol_common.Srcloc.TYPES
open Text

(* cobc support does not currently work because, when replacing is
   done, the location of the new token is the location of the
   replacing token. Since that location is used to display #area_a
   directives, such directives may wrongly be added or removed.

   TODO:To fix this, the location of the first replaced token should
   be kept for the first replacing token.  *)


type pos = {
  file : string ;
  line : int ;
  char : int ;

  (* cobc specific stuff *)
  divisions_seen : int ;   (* ignore comments if == 1 *)
  ignore_paragraph : bool ; (* are we ignoring current paragraph ? *)
}

(* if cobc && divisions_seen == 1, these paragraphs should be ignored *)
let cobc_ignore_paragraph_after = function
  | "AUTHOR"
  | "DATE-WRITTEN"
  | "DATE-MODIFIED"
  | "DATE-COMPILED"
  | "INSTALLATION"
  | "REMARKS"
  | "SECURITY"-> true
  | _ -> false

(* TODO: if two following tokens are of type TextWord, we might
   attempt to merge them, disregarding the location of the second
   token. Maybe it is not necessary, practice will tell. *)
let string_of_text ?(cobc=false) ?(max_line_gap=1) text =

  let b = Buffer.create 65_000 in
  let rec move_to_loc pos loc =
    let start_pos =
      (* CHECKME: Give the actual position of the first token leading to a
         source location; behavior when in copied/replaced pseudotext needs to
         be checked. *)
      Cobol_common.Srcloc.start_pos loc
    in
    let Lexing.{ pos_lnum ; pos_fname; pos_bol ; pos_cnum ; _ } = start_pos in
    let pos_lnum = pos_lnum - 1 in
    let pos_cnum = pos_cnum - pos_bol in
    (*    Printf.eprintf "pos_lnum = %d, pos_cnum = %d\n%!" pos_lnum pos_cnum; *)
    if pos_fname <> pos.file then begin
      if pos.char <> 0 then Buffer.add_char b '\n';
      Printf.bprintf b "#line %d \"%s\"\n" (pos_lnum+1) pos_fname;
      let pos = { pos with file = pos_fname ; line = pos_lnum ; char = 0 } in
      move_to_loc pos loc
    end else
    if pos_lnum < pos.line
    || ( pos_lnum = pos.line &&  pos_cnum < pos.char )
    || pos_lnum > pos.line + max_line_gap
    then begin
      if pos.char <> 0 then Buffer.add_char b '\n';
      Printf.bprintf b "#line %d \"%s\"\n" (pos_lnum+1) pos_fname;
      let pos = { pos with file = pos_fname ; line = pos_lnum ; char = 0 } in
      move_to_loc pos loc
    end else
    if pos_lnum > pos.line then begin
      Buffer.add_char b '\n';
      let pos = { pos with file = pos_fname ; line = pos.line+1 ; char = 0 } in
      move_to_loc pos loc
    end else
    if cobc && pos.char == 0 &&
       (pos_cnum >= 7 && pos_cnum < 11) then begin
      Buffer.add_string b "\n#area_a\n";
      let pos = { pos with char = pos.char + 6; ignore_paragraph = false } in
      move_to_loc pos loc
    end else
    if pos_cnum > pos.char then begin
      Buffer.add_char b ' ';
      let pos = { pos with char = pos.char + 1 } in
      move_to_loc pos loc
    end else
      pos (* TODO *)
  in
  let advance_of pos s =
    { pos with char = pos.char + String.length s } (* Handle newlines *)
  in
  let string_of_word chstr =
    match chstr with
    | TextWord str ->
        str
    | CDirWord str ->
        str
    | Alphanum { knd; qte; str } ->
        let c = Text.char_of_quotation qte in
        let prefix = Text.prefix_of_literal_kind knd in
        Printf.sprintf "%s%c%s%c"
          prefix c str c
    | AlphanumPrefix { knd; qte; str } ->
        let c = Text.char_of_quotation qte in
        let prefix = Text.prefix_of_literal_kind knd in
        Printf.sprintf "%s%c%s"
          prefix c str
    | Pseudo pl ->
        Printf.sprintf "==%s=="
          ( String.concat " " @@
            List.map (fun { payload = pseudo; _ } ->
                match pseudo with
                | PseudoWord strs ->
                    String.concat "" @@
                    List.map begin function
                      | { payload = PwText s | PwDelim (s, _); _ } -> s
                    end strs
                | PseudoAlphanum { knd; qte; str } ->
                    let c = Text.char_of_quotation qte in
                    let prefix = Text.prefix_of_literal_kind knd in
                    Printf.sprintf "%s%c%s%c"
                      prefix c str c
              ) pl)
    | Eof -> ""
  in
  let rec iter pos text =
    match text with
    | [] -> Buffer.add_char b '\n'; Buffer.contents b
    | chstr_loc :: text ->
        let chstr = Cobol_common.Srcloc.payload chstr_loc in
        let loc = Cobol_common.Srcloc.loc chstr_loc in
        let pos = move_to_loc pos loc in
        let s = string_of_word chstr in
        let pos =
          if s = "DIVISION" then
            { pos with divisions_seen = pos.divisions_seen + 1 }
          else
          if cobc && pos.divisions_seen = 1 &&
             cobc_ignore_paragraph_after s then
            { pos with ignore_paragraph = true; }
          else
            pos
        in
        let s = if pos.ignore_paragraph then "" else s in
        Buffer.add_string b s;

        (* ONLY FOR DEBUG: display locations on stderr
           begin
           let Lexing.{ pos_lnum ; pos_fname; pos_bol ; pos_cnum ; _ } =
            fst ( List.hd loc ) in
           let pos_lnum = pos_lnum - 1 in
           let pos_cnum = pos_cnum - pos_bol in
           Printf.eprintf "{ pos_fname = %s, pos_lnum = %d, pos_cnum = %d} -> [%s]\n%!"
            pos_fname pos_lnum pos_cnum s
           end;
        *)

        let pos = advance_of pos s in
        iter pos text
  in
  iter {
    file = "" ; line = 0 ; char = 0;
    divisions_seen = 0; ignore_paragraph = false;
  } text
