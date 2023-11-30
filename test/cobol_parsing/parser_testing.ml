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

module DIAGS = Cobol_common.Diagnostics
module StrMap = EzCompat.StringMap

let preproc
    ?(filename = "prog.cob")
    ?(source_format = Cobol_config.(SF SFFixed))
    contents
  =
  Cobol_common.Srcloc.TESTING.register_file_contents ~filename contents;
  String { filename; contents } |>
  Cobol_preproc.preprocessor
    ~options:Cobol_preproc.Options.{
        default with
        libpath = [];
        source_format
      }

let show_parsed_tokens ?(verbose = false) ?source_format ?filename contents =
  let DIAGS.{ result = WithArtifacts (_, { tokens; _ }); _ } =
    preproc ?source_format ?filename contents |>
    Cobol_parser.parse_with_artifacts
      ~options:Cobol_parser.Options.{
          default with
          verbose;
          recovery = EnableRecovery { silence_benign_recoveries = true };
        }
  in
  Cobol_parser.INTERNAL.pp_tokens Fmt.stdout (Lazy.force tokens)

let show_diagnostics ?(verbose = false) ?source_format ?filename contents =
  preproc ?source_format ?filename contents |>
  Cobol_parser.parse_simple
    ~options:Cobol_parser.Options.{
        default with
        verbose;
        recovery = EnableRecovery { silence_benign_recoveries = true };
      } |>
  DIAGS.forget_result |>
  DIAGS.Set.pp Fmt.stdout

(* --- *)

(** Structure returned by {!extract_position_markers} below. *)
type positions =
  {
    pos_anonymous: position list;
    pos_map: position StrMap.t;
  }
and position =
  {
    line: int;
    char: int;
    cnum: int;
  }

let position_marker = "_|_\\|_|[0-9a-zA-Z-+]+|_"
let position_or_newline_regexp = Str.(regexp @@ position_marker ^ "\\|\n")

(** [extract_position_markers text] records and removes any cursor position
    marker from [text], and returns the resulting text along with a set of
    cursor positions.

    Anonymous markers are denoted {[_|_]}. They are listed in order of
    appearance in [text] ([pos_anonymous]).

    Named markers are denoted {[_|some-name|_]}, where {[some-name]} may
    comprise alphanumeric and [+] or [-] characters.  They are recorded in
    [pos_map]. *)
let extract_position_markers
    ?(with_start_pos = true) ?(with_end_pos = true)
    text =
  let splits = Str.full_split position_or_newline_regexp text in
  let positions =
    if with_start_pos
    then [{ line = 0; char = 0; cnum = 0 }, None]
    else []
  in
  let acc, line, char, cnum, positions =
    List.fold_left begin fun (acc, line, char, cnum, positions) -> function
      | Str.Text t ->
          let len = String.length t in
          t :: acc, line, char + len, cnum + len, positions
      | Str.Delim "\n" ->
          "\n" :: acc, succ line, 0, cnum + 1, positions
      | Str.Delim "_|_" ->
          acc, line, char, cnum, ({ line; char; cnum },
                                  None) :: positions
      | Str.Delim d ->
          let position_ident = Scanf.sscanf d "_|%s@|_" Fun.id in
          acc, line, char, cnum, ({ line; char; cnum },
                                  Some position_ident) :: positions
    end ([], 0, 0, 0, positions) splits
  in
  let positions =
    if with_end_pos
    then ({ line; char; cnum }, None) :: positions
    else positions
  in
  String.concat "" (List.rev acc),
  List.fold_left begin fun acc (pos, ident) ->
    match ident with
    | None -> { acc with pos_anonymous = pos :: acc.pos_anonymous }
    | Some id -> { acc with pos_map = StrMap.add id pos acc.pos_map }
  end { pos_anonymous = []; pos_map = StrMap.empty } positions

let insert_periodic_position_markers ?(period = 1) prog =
  let rec aux acc prog =
    if String.length prog <= period
    then
      String.concat "_|_" (List.rev (prog :: acc))
    else
      let chunk, rem = EzString.before prog period,
                       EzString.after prog (period - 1) in
      aux (chunk :: acc) rem
  in
  aux [] prog

let pairwise positions =
  List.(combine (rev (tl (rev positions))) (tl positions))

let triplewise positions =
  List.(combine (rev (tl (tl (rev positions)))) (pairwise (tl positions)))

(* --- *)

(** Note: won't show detailed source locations as the openned file is neither
    actually on disk nor registered via {!Srcloc.register_file_contents}. *)
let rewindable_parse
    ?(verbose = false)
    ?(source_format = Cobol_config.(SF SFFixed))
    ?config
    prog
  =
  let DIAGS.{ result = Only ptree, rewinder; diags } =
    String { filename = "prog.cob"; contents = prog } |>
    Cobol_preproc.preprocessor
      ~options:Cobol_preproc.Options.{
          verbose; libpath = []; source_format;
          config = Option.value config ~default:default.config;
        } |>
    Cobol_parser.rewindable_parse_simple
      ~options:Cobol_parser.Options.{
          default with
          verbose; recovery = DisableRecovery;
          config = Option.value config ~default:default.config;
        }
  in
  ptree, diags, rewinder

(** Note: won't show detailed source locations as the openned file is neither
    actually on disk nor registered via {!Srcloc.register_file_contents}. *)
let rewind_n_parse ~f rewinder { line; char; _ } preproc_rewind =
  let DIAGS.{ result = Only ptree, rewinder; diags } =
    Cobol_parser.rewind_and_parse rewinder preproc_rewind
      ~position:(Indexed { line; char })
  in
  f ptree diags;
  rewinder


(** [iteratively_append_chunks ?config ~f (prog, positions)] starts a rewindable
    parser on a first chunk of input (until the first position in
    [positions.pos_anonymous]), and then iteralively appends the remaining
    chunks (from one position to the next).  [f] is called after each successive
    chunk has been parsed, with chunk number and total number of chunks as first
    and second arguments, respectively.

    Note: won't show detailed source locations as the openned file is neither
    actually on disk nor registered via {!Srcloc.register_file_contents}. *)
let iteratively_append_chunks ?config ~f (prog, positions) =
  let _, _, rewinder =
    rewindable_parse ?config @@            (* start with first chunk of input *)
    EzString.before prog (List.hd positions.pos_anonymous).cnum
  in
  let num_chunks = List.length positions.pos_anonymous - 1 in
  ignore @@ List.fold_left begin fun (i, rewinder) (pos, next_pos) ->
    let prog = EzString.before prog next_pos.cnum in
    Pretty.out "Appending chunk %u/%u @@ %d:%d-%d:%d (%a)@."
      i num_chunks
      pos.line pos.char next_pos.line next_pos.char
      Fmt.(truncated ~max:30)
      Pretty.(to_string "%S" @@ EzString.after prog (pos.cnum - 1));
    succ i,
    rewind_n_parse ~f:(f i num_chunks) rewinder pos
      (Cobol_preproc.reset_preprocessor_for_string prog)
  end (1, rewinder) (pairwise positions.pos_anonymous)


(** [iteratively_append_chunks_stuttering ?config ~f (prog, positions)] starts a
    rewindable parser on a first chunk of input (until the first position in
    [positions.pos_anonymous]), and then iteralively appends the remaining pairs
    of chunks (from one position to the one after the next).  Each pair of chunk
    (but the last) is added in three steps: first the entire pair is appended,
    and then the second chunk of the pair is cut back.  [f] is called after each
    successive chunk has been parsed, with chunk-pair number and total number of
    chunks-pairs as first and second arguments, respectively.

    Note: won't show detailed source locations as the openned file is neither
    actually on disk nor registered via {!Srcloc.register_file_contents}. *)
let iteratively_append_chunks_stuttering ?config ~f
    (prog, positions) =
  let _, _, rewinder =
    rewindable_parse ?config @@            (* start with first chunk of input *)
    EzString.before prog (List.hd positions.pos_anonymous).cnum
  in
  let num_chunks = List.length positions.pos_anonymous - 2 in
  ignore @@
  List.fold_left begin fun (i, rewinder) (pos, (next_pos_1, next_pos_2)) ->
    let prog = EzString.before prog next_pos_2.cnum in
    Pretty.out "Appending chunk %u/%u @@ %d:%d-%d:%d (%a)@."
      i num_chunks
      pos.line pos.char next_pos_2.line next_pos_2.char
      Fmt.(truncated ~max:30)
      Pretty.(to_string "%S" @@ EzString.after prog (pos.cnum - 1));
    let rewinder =
      rewind_n_parse ~f:(f i num_chunks) rewinder pos
        (Cobol_preproc.reset_preprocessor_for_string prog)
    in
    let rewinder =
      if i < num_chunks then begin
        let prog'= EzString.before prog next_pos_1.cnum in
        Pretty.out "Cutting chunk %u/%u back @@ %d:%d-%d:%d (%a)@."
          i num_chunks
          pos.line pos.char next_pos_1.line next_pos_1.char
          Fmt.(truncated ~max:30)
          Pretty.(to_string "%S" @@ EzString.after prog' (pos.cnum - 1));
        rewind_n_parse ~f:(f i num_chunks) rewinder next_pos_1
          (Cobol_preproc.reset_preprocessor_for_string prog')
      end else rewinder
    in
    succ i, rewinder
  end (1, rewinder) (triplewise positions.pos_anonymous)


(** [simulate_cut_n_paste ?config ~f (prog, positions)] starts a rewindable
    parser on [prog], and then repeatedly cuts a chunk [c] form the input (from
    one position to the next in [positions.pos_anonymous]), try to parse, and
    paste [c] back in its original place.  [f0] is called once at the very
    beginning with the results obtained by parsing the original program [prog].
    [f] is called after a chunk has been cut and pasted back.

    Note: won't show detailed source locations as the openned file is neither
    actually on disk nor registered via {!Srcloc.register_file_contents}. *)
let simulate_cut_n_paste ?config ~f0 ~f ?verbose ?(repeat = 1)
    (prog, positions) =
  Random.init 42;
  let ptree0, diags, rewinder = rewindable_parse ?verbose ?config prog in
  f0 ~ptree0 diags;
  let positions = Array.of_list positions.pos_anonymous in
  let num_chunks = Array.length positions - 1 in
  let rec loop i rewinder =
    if i < repeat then begin
      let chunk_num = Random.int num_chunks in                (* pick a chunk *)
      let pos = positions.(chunk_num)
      and next_pos = positions.(chunk_num + 1) in
      let chunk =
        EzString.(after (before prog next_pos.cnum) (pos.cnum - 1)) in
      Pretty.out "Cutting chunk %u/%u @@ %d:%d(%d)-%d:%d(%d) (%a)@."
        chunk_num num_chunks
        pos.line pos.char pos.cnum
        next_pos.line next_pos.char next_pos.cnum
        Fmt.(truncated ~max:30) Pretty.(to_string "%S" chunk);
      let prog_prefix = EzString.before prog pos.cnum
      and prog_suffix = EzString.after prog (next_pos.cnum - 1) in
      let rewinder =
        rewind_n_parse ~f:(fun _ _ -> ()) rewinder pos @@
        Cobol_preproc.reset_preprocessor_for_string @@
        prog_prefix ^ prog_suffix
      in
      Pretty.out "Putting it back@.";
      let rewinder =
        rewind_n_parse ~f:(f chunk_num num_chunks ~ptree0) rewinder pos @@
        Cobol_preproc.reset_preprocessor_for_string prog
      in
      loop (succ i) rewinder
    end
  in
  loop 0 rewinder
