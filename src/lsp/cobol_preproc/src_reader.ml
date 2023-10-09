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

open Ez_file.V1
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Text.TYPES

type 'k reader = 'k Src_lexing.state * Lexing.lexbuf
type 'k line = Line: 'k reader * text -> 'k line
type t = Plx: 'k reader -> t                                           [@@unboxed]

let diags (Plx (pl, _)) = Src_lexing.diagnostics pl
let position (Plx (_, lexbuf)) = lexbuf.Lexing.lex_curr_p
let comments (Plx (pl, _)) = Src_lexing.comments pl
let source_format (Plx (pl, _)) = Src_format.SF (Src_lexing.source_format pl)
let newline_cnums (Plx (pl, _)) = Src_lexing.newline_cnums pl

let chunks_reader lexer =
  let rec next_line (state, lexbuf) =
    let state, pseutoks = lexer state lexbuf in
    match pseutoks with
    | [] -> next_line (state, lexbuf)                      (* skip blank lines *)
    | _ -> Line ((state, lexbuf), pseutoks)
  in
  next_line

let next_chunk (Plx pl) =
  let Line (pl, text) = chunks_reader Src_lexer.line pl in
  Plx pl, text

let fold_chunks (Plx pl) f acc =
  let rec aux pl acc = match chunks_reader Src_lexer.line pl with
    | Line (_, [{ payload = Eof; _}]) -> acc
    | Line (pl, text) -> aux pl (f text acc)
  in
  aux pl acc

(* let print_chunks ppf pl = *)
(*   fold_chunks pl (fun t () -> Pretty.print ppf "%a@\n" Text.pp_text t) () *)

(* --- *)

(** [fold_source_lines pl ~f acc] applies [f line_number line acc] for each
    successive line [line] of the input lexed by [pl].  [line_number] gives the
    line number for [line] (starting at [1]).  [line] is given empty to [f] if
    it corresponds to empty line in the input, or was a line continuation. *)
let fold_lines pl ~f acc =
  let tok_lnum tok =
    (* On source text, which is NOT manipulated, we only have lexical locations,
       so using [start_pos] is enough. *)
    (Cobol_common.Srcloc.start_pos ~@tok).pos_lnum
  in
  let spit_empty_lines ~until_lnum cur_lnum acc =
    let rec aux cur_lnum acc =
      if cur_lnum < until_lnum
      then aux (succ cur_lnum) (f cur_lnum [] acc)
      else acc
    in
    aux cur_lnum acc
  in
  let rec spit_chunk chunk (acc, cur_lnum, cur_prefix) =
    match
      Cobol_common.Basics.LIST.split_at_first ~prefix:`Same ~where:`Before
        (fun tok -> tok_lnum tok > cur_lnum) chunk
    with
    | Error () ->                                    (* still on the same line *)
        (acc, cur_lnum, cur_prefix @ chunk)
    | Ok (prefix, []) ->           (* should not happen (in case, just append) *)
        (acc, cur_lnum, cur_prefix @ prefix)
    | Ok (prefix, (tok :: _ as suffix)) ->                (* terminating a line *)
        let acc = f cur_lnum (cur_prefix @ prefix) acc in
        let new_lnum = tok_lnum tok in
        let acc = spit_empty_lines ~until_lnum:new_lnum (succ cur_lnum) acc in
        spit_chunk suffix (acc, new_lnum, [])
  in
  let acc, last_lnum, tail = fold_chunks pl spit_chunk (acc, 1, []) in
  match tail with                       (* fold on the last line upon exit... *)
  | [] | { payload = Eof; _ } :: _ -> acc (* ... if non-empty *)
  | _ -> f last_lnum tail acc

let print_lines ppf pl =
  fold_lines pl ~f:(fun _ line () -> Pretty.print ppf "%a@\n" Text.pp_text line) ()

(* --- *)

(* Change of source format *)

let with_source_format
  : 'k Src_format.source_format with_loc -> t -> t
  = fun format ((Plx (s, lexbuf)) as pl) ->
    if Src_format.equal (Src_lexing.source_format s) ~&format
    then pl
    else match Src_lexing.change_source_format s format with
      | Ok s -> Plx (s, lexbuf)
      | Error s -> Plx (s, lexbuf)

(* --- *)

let make make_lexing ?filename ~source_format input =
  let Src_format.SF source_format = source_format in
  (* Be sure to provide position informations *)
  let lexbuf = make_lexing ?with_positions:(Some true) input in
  Option.iter (Lexing.set_filename lexbuf) filename;
  Plx (Src_lexing.init_state source_format, lexbuf)

let from_string = make Lexing.from_string
let from_channel = make Lexing.from_channel
let from_file ~source_format filename : t =
  from_string ~source_format ~filename (EzFile.read_file filename)

(** Note: If given, assumes [position] corresponds to the beginning of the
    input, which {e must} also be at the beginning of a line.  If absent,
    restarts from first position.  File name is kept from the previous input. *)
let restart make_lexing ?position input (Plx (s, prev_lexbuf)) =
  let lexbuf = make_lexing ?with_positions:(Some true) input in
  let pos_fname = match position with
    | Some p ->
        Lexing.set_position lexbuf p;
        p.Lexing.pos_fname
    | None ->
        prev_lexbuf.Lexing.lex_curr_p.pos_fname
  in
  Lexing.set_filename lexbuf pos_fname;
  Plx (s, lexbuf)

let restart_on_string = restart Lexing.from_string
let restart_on_channel = restart Lexing.from_channel
let restart_on_file ?position filename =
  restart_on_string ?position (EzFile.read_file filename)
