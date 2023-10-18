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

module DIAGS = Cobol_common.Diagnostics

type 'k reader = 'k Src_lexing.state * Lexing.lexbuf
type 'k line = Line: 'k reader * text -> 'k line
type t = Plx: 'k reader -> t                                           [@@unboxed]

type error =
  | Malformed_or_unknown_compiler_directive of srcloc
  | Unknown_source_format of string * srcloc

let error_diagnostic = function
  | Malformed_or_unknown_compiler_directive loc ->
      DIAGS.One.error ~loc "Malformed@ or@ unknown@ compiler@ directive"
  | Unknown_source_format (f, loc) ->
      DIAGS.One.error ~loc "Unknown@ source@ format@ `%s'" f

(* --- *)

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

(* let print_chunks ppf pl = *)
(*   fold_chunks pl (fun t () -> Pretty.print ppf "%a@\n" Text.pp_text t) () *)

(* Change of source format *)

let with_source_format: Src_format.any with_loc -> t -> t =
  fun { payload = SF format; loc } ((Plx (s, lexbuf)) as pl) ->
  if Src_format.equal (Src_lexing.source_format s) format
  then pl
  else match Src_lexing.change_source_format s (format &@ loc) with
    | Ok s -> Plx (s, lexbuf)
    | Error s -> Plx (s, lexbuf)

(* --- *)

(** [lookup_compiler_directive chunk] searches for a compiler-directive
    text-word (that starts with either `{v >> v}' or `{v $ v}') in the given
    chunk of source text.

    Returns [Ok (prefix, cdir_text)] if a compiler directive is recognised,
    where [cdir_text] is guaranteed to start with a compiler-directive word on a
    line [l] and terminates at the end [l]. *)
(* Note: {!next_chunk} never outputs compiler-directive text-words in positions
   other than the first two.  Such a chunk also terminates at the end of the
   source line as it cannot be continued (contrary to normal source lines). *)
let lookup_compiler_directive: Text.text -> _ = function
  |      t :: _ as text  when Text.cdirp t -> Ok ([ ], text)
  | p :: (t :: _ as text) when Text.cdirp t -> Ok ([p], text)
  | _ -> Error `NotCDir

let decode_compiler_directive ~dialect compdir_text =
  (* Here, [compdir_text] is never empty as it's known to start with a compiler
     directive marker `>>` (or `$` for MF-style directives), so we should always
     have a loc: *)
  let supplier = Text_supplier.cdtoks_of_text_supplier compdir_text in
  let loc = Option.get @@ Cobol_common.Srcloc.concat_locs compdir_text in
  let start_pos = Cobol_common.Srcloc.start_pos loc in
  let parser = Compdir_grammar.Incremental.compiler_directive start_pos in
  let raw_loc = Cobol_common.Srcloc.raw in
  let open Preproc_directives in
  match Compdir_grammar.MenhirInterpreter.loop supplier parser with
  | Source_format_is_free lexloc ->
      let sf = Src_format.from_config Cobol_config.SFFree in
      Ok (CDirSource (sf &@ raw_loc lexloc) &@ loc)
  | Source_format_is (format, lexloc)
  | Set_sourceformat (format, lexloc) ->
      (match Src_format.decypher ~dialect format with
       | Ok sf ->
           Ok (Preproc_directives.CDirSource (sf &@ raw_loc lexloc) &@ loc)
       | Error (`SFUnknown f) ->
           Error (Unknown_source_format (f, raw_loc lexloc)))
  | Set (string, lexloc) ->
      Ok (Preproc_directives.CDirSet (string &@ raw_loc lexloc) &@ loc)
  | exception Compdir_grammar.Error ->
      Error (Malformed_or_unknown_compiler_directive loc)

let try_compiler_directive ~dialect text =
  match lookup_compiler_directive text with
  | Error `NotCDir ->
      Ok None
  | Ok (prefix, compdir_text) ->
      match decode_compiler_directive ~dialect compdir_text with
      | Error e ->
          Error (prefix, e, compdir_text)
      | Ok compdir ->
          Ok (Some (prefix, compdir, compdir_text))

let fold_chunks
    ~dialect
    ?(skip_compiler_directives_text = false)
    ?on_compiler_directive
    ~f
    pl acc =
  let rec aux pl acc = match next_chunk pl with
    | _pl, { payload = Eof; _} :: _ ->
        acc
    | pl, text ->
        match try_compiler_directive ~dialect text with
        | Ok None ->
            aux pl (f text acc)
        | Ok Some (prefix, compdir, text) ->
            let acc = f prefix acc in
            let acc =
              if skip_compiler_directives_text
              then acc
              else f text acc
            in
            let acc = match on_compiler_directive with
              | None -> acc
              | Some f -> f compdir acc
            in
            aux (apply_compdir compdir pl) acc
        | Error (prefix, _error, text) ->                     (* ignore error? *)
            let acc = f prefix acc in
            if skip_compiler_directives_text
            then aux pl acc
            else aux pl (f text acc)
  and apply_compdir { payload = compdir; _ } = match compdir with
    | CDirSource sf -> with_source_format sf
    | CDirSet _ -> Fun.id                                            (* ignore *)
  in
  aux pl acc

(* --- *)

(** [fold_lines ~dialect ~skip_compiler_directives_text ~on_compiler_directive
    ~f pl acc] applies [f line_number line acc] for each successive line [line]
    of the input lexed by [pl].  [line_number] gives the line number for [line]
    (starting at [1]).  [line] is given empty to [f] if it corresponds to an
    empty line in the input, or was a line continuation.

    When given, [on_compiler_directive] is called {e after} [f] has been fed
    with the text of a compiler directive, with the same line number.

    When set, [skip_compiler_directives_text] ([false] by default) prevents the
    text of compiler directives from being fed to [f].  If given,
    [on_compiler_directive] is called as if the text had been fed to [f]. *)
let fold_lines
    ~dialect
    ?skip_compiler_directives_text
    ?on_compiler_directive
    ~f pl acc =
  let loc_lnum { loc; _ } =
    (* On source text, which is NOT manipulated, we only have lexical locations,
       so using [start_pos] is enough. *)
    (Cobol_common.Srcloc.start_pos loc).pos_lnum
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
        (fun tok -> loc_lnum tok > cur_lnum) chunk
    with
    | Error () ->                                    (* still on the same line *)
        (acc, cur_lnum, cur_prefix @ chunk)
    | Ok (prefix, []) ->           (* should not happen (in case, just append) *)
        (acc, cur_lnum, cur_prefix @ prefix)
    | Ok (prefix, (tok :: _ as suffix)) ->                (* terminating a line *)
        let acc = f cur_lnum (cur_prefix @ prefix) acc in
        let new_lnum = loc_lnum tok in
        let acc = spit_empty_lines ~until_lnum:new_lnum (succ cur_lnum) acc in
        spit_chunk suffix (acc, new_lnum, [])
  in
  let spit_compdir f' cdir (acc, cur_lnum, prefix) =
    let acc = if prefix <> [] then f cur_lnum prefix acc else acc in  (* flush *)
    let new_lnum = loc_lnum cdir in
    let acc = spit_empty_lines ~until_lnum:new_lnum cur_lnum acc in
    let acc = f' new_lnum cdir acc in    (* call [f'] at end of corresp. line *)
    acc, succ new_lnum, []
  in
  let acc, last_lnum, tail =
    fold_chunks ~dialect pl ~f:spit_chunk (acc, 1, [])
      ?skip_compiler_directives_text
      ?on_compiler_directive:(Option.map spit_compdir on_compiler_directive)
  in
  match tail with                       (* fold on the last line upon exit... *)
  | [] | { payload = Eof; _ } :: _ -> acc (* ... if non-empty *)
  | _ -> f last_lnum tail acc

let print_lines ~dialect ?skip_compiler_directives_text ppf pl =
  fold_lines ~dialect ?skip_compiler_directives_text pl ()
    ~f:(fun _ line () -> Pretty.print ppf "%a@\n" Text.pp_text line)

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
