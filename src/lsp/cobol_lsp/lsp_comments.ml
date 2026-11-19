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

(** Text edits that toggle comments on a set of lines.

    Comment lines are recognized from their raw text and source format, rather
    than from parser outputs that may be outdated or missing (e.g, for
    copybooks). *)

open Lsp.Types

module SF = Cobol_preproc.Src_format

(* --- *)

(** Ascending [(line, format)] pairs, where [format] applies from (0-based)
    [line] onward. *)
type format_changes = (int * SF.any) list

let format_changes ~platform ~filename
    ~(options: Cobol_preproc.Options.preproc_options) text : format_changes =
  let Cobol_preproc.Options.{ config; source_format; mf_comments; _ } =
    options in
  let src_options =
    Cobol_preproc.Src_options.{ tab_stops = config.tab_stops; mf_comments } in
  (* [l] is the 1-based number of the last line read: [0] for the initial
     format, and the line of the directive for a change, which is also the
     0-based number of the line the new format applies from. *)
  let _, rev_changes =
    Cobol_preproc.fold_source_lines ~platform ~source_format ~src_options
      ~dialect:(Cobol_config.dialect config)
      ~on_change_of_source_format:(fun sf (l, changes) -> l, (l, sf) :: changes)
      ~f:(fun l _ (_, changes) -> l, changes)
      (Cobol_preproc.Input.string ~filename text)
      (0, [])
  in
  List.rev rev_changes

let format_at_line (changes: format_changes) line =
  List.fold_left begin fun current (l, sf) ->
    if l <= line then sf else current
  end (snd (List.hd changes)) changes

(* --- *)

(** Location of a visual column in a line. *)
type column_spot =
  | Char of int    (** byte offset of the character at the visual column *)
  | Tab of { offset: int; start_col: int; next_col: int }
  (** tab at byte [offset] that spans the visual column ([start_col <= col <
      next_col]) *)
  | Short                         (** the line ends before the visual column *)

(** [locate_column ~tab_stops line col] locates the visual column [col]
    (1-based) in [line].  UTF-8 continuation bytes do not count as columns. *)
let locate_column ~tab_stops line col =
  let len = String.length line in
  let rec aux i c =
    if i >= len then Short
    else match line.[i] with
      | '\t' ->
          let next_col = SF.next_tab_stop ~tab_stops c in
          if c <= col && col < next_col
          then Tab { offset = i; start_col = c; next_col }
          else aux (succ i) next_col
      | '\128'..'\191' ->                         (* UTF-8 continuation byte *)
          aux (succ i) c
      | _ when c = col ->
          Char i
      | _ ->
          aux (succ i) (succ c)
  in
  aux 0 1

(* --- *)

(** Edit on a single line, expressed in byte offsets. *)
type line_edit =
  {
    start_offset: int;
    end_offset: int;
    new_text: string;
  }

let replace ~at ?(len = 1) new_text =
  { start_offset = at; end_offset = at + len; new_text }

let insert ~at new_text =
  replace ~at ~len:0 new_text

let is_blank_char = function ' ' | '\t' -> true | _ -> false

let first_non_blank ?(from = 0) line =
  let len = String.length line in
  let rec aux i =
    if i >= len then None
    else if is_blank_char line.[i] then aux (succ i)
    else Some i
  in
  aux from

let has_prefix_at line ~at prefix =
  let plen = String.length prefix in
  at + plen <= String.length line && String.sub line at plen = prefix

type line_status =
  | Blank
  | Code of { comment: line_edit option }
  (** [None] in free-like formats, where the marker position depends on the
      other selected lines *)
  | Comment of { uncomment: line_edit }

type line_kind =
  | Free_like of { col1_comments: bool }    (* commented out with [*>] markers;
                                               X/Open, CRT and Terminal formats
                                               also have [*] or [/] comment
                                               lines *)
  | Indicator_at of int                            (* column of the indicator *)

let line_kind: SF.any -> line_kind = function
  | SF (NoIndic, _) -> Free_like { col1_comments = false }
  | SF ((XOpenIndic | CRTIndic | TrmIndic), _) ->
      Free_like { col1_comments = true }
  | SF (FixedIndic, _) -> Indicator_at 7
  | SF (CBLXIndic, _) -> Indicator_at 1

(** Removes the [*>] at byte [at], and one following space. *)
let remove_floating_marker line ~at =
  let len = if has_prefix_at line ~at:(at + 2) " " then 3 else 2 in
  replace ~at ~len ""

let free_like_status ~col1_comments line =
  match first_non_blank line with
  | None ->
      Blank
  | Some i when has_prefix_at line ~at:i "*>" ->
      Comment { uncomment = remove_floating_marker line ~at:i }
  | Some 0 when col1_comments && (line.[0] = '*' || line.[0] = '/') ->
      Comment { uncomment = replace ~at:0 "" }
  | Some _ ->
      Code { comment = None }

let indicator_status ~tab_stops ~mf_comments ~indic_col sf line =
  let SF.SF sf' = sf in
  if mf_comments && SF.mf_comment_applies sf' &&
     String.length line > 0 && line.[0] = '*'
  then Comment { uncomment = replace ~at:0 " " }
  else match locate_column ~tab_stops line indic_col with
    | Short ->
        Blank
    | Tab { offset; start_col; next_col } ->
        (* Expand the tab around the indicator, keeping the rest aligned. *)
        if first_non_blank ~from:(succ offset) line = None then Blank else
          let spaces n = String.make n ' ' in
          Code { comment = Some (replace ~at:offset @@
                                 spaces (indic_col - start_col) ^ "*" ^
                                 spaces (next_col - indic_col - 1)) }
    | Char i ->
        let next = if i + 1 < String.length line then line.[i + 1] else ' ' in
        match line.[i], next, first_non_blank ~from:i line with
        | _, _, None ->
            Blank
        | '*', ('-' | 'd' | 'D' | '$'), _ ->
            (* [*] inserted before a non-blank indicator (see below) *)
            Comment { uncomment = replace ~at:i "" }
        | '*', '>', _ when has_prefix_at line ~at:i "*>>" ->
            (* Commented-out [>>] directive: blanking the [*] restores it, or
               moves it from column 7 to 8, which is equivalent. *)
            Comment { uncomment = replace ~at:i " " }
        | _, _, Some j when has_prefix_at line ~at:j "*>" ->
            (* floating comment: blank the marker to preserve alignment *)
            Comment { uncomment = replace ~at:j ~len:2 "  " }
        | ('*' | '/'), _, _ ->
            Comment { uncomment = replace ~at:i " " }
        | ' ', _, _ ->
            Code { comment = Some (replace ~at:i "*") }
        | _ ->
            (* Non-blank indicator ([-], [D], [$], [>]): insert the [*] so
               uncommenting restores it. *)
            Code { comment = Some (insert ~at:i "*") }

let line_status ~tab_stops ~mf_comments sf line =
  match line_kind sf with
  | Free_like { col1_comments } ->
      free_like_status ~col1_comments line
  | Indicator_at indic_col ->
      indicator_status ~tab_stops ~mf_comments ~indic_col sf line

(* --- *)

(** Ascending (0-based) numbers of the lines covered by [ranges].  A
    non-empty range that ends at the start of a line does not cover it. *)
let selected_lines (ranges: Range.t list) =
  List.fold_left begin fun acc Range.{ start; end_ } ->
    let start, end_ =
      if (start.line, start.character) <= (end_.line, end_.character)
      then start, end_ else end_, start
    in
    let last =
      if end_.character = 0 && end_.line > start.line
      then end_.line - 1
      else end_.line
    in
    let rec add acc l = if l > last then acc else add (l :: acc) (succ l) in
    add acc start.line
  end [] ranges |>
  List.sort_uniq Int.compare

let split_lines text =
  let strip_cr l =
    let len = String.length l in
    if len > 0 && l.[len - 1] = '\r' then String.sub l 0 (len - 1) else l
  in
  Array.map strip_cr @@ Array.of_list @@ String.split_on_char '\n' text

(** Number of characters in the first [len] bytes of [line]. *)
let char_offset ~position_encoding line len =
  match position_encoding with
  | `UTF8 ->
      len
  | `UTF16 ->
      let rec aux i n =
        if i >= len then n
        else
          let c = Char.code line.[i] in
          if c < 0x80 then aux (i + 1) (n + 1)
          else if c < 0xC0 then aux (i + 1) n           (* continuation byte *)
          else if c < 0xF0 then aux (i + 1) (n + 1)
          else aux (i + 1) (n + 2)                      (* surrogate pair *)
      in
      aux 0 0

let text_edit ~position_encoding ~line_num line { start_offset; end_offset;
                                                  new_text } =
  let pos o =
    Position.create ~line:line_num
      ~character:(char_offset ~position_encoding line o)
  in
  TextEdit.create ~newText:new_text
    ~range:(Range.create ~start:(pos start_offset) ~end_:(pos end_offset))

(** Edits that toggle comments on the lines of [text] covered by [ranges].

    If every non-blank selected line is a comment, all are uncommented.
    Otherwise all are commented out: with an indicator area, comment lines are
    left as is; in other formats, [*>] markers are inserted at the smallest
    indentation, nested on comment lines so toggling back preserves them.

    Toggling a source format directive changes the format of later lines, so
    toggling twice may not restore the original text. *)
let edits ~platform ~options ~filename ~position_encoding text ranges =
  let lines = split_lines text in
  let tab_stops = options.Cobol_preproc.Options.config.tab_stops
  and mf_comments = options.mf_comments in
  let format_changes = format_changes ~platform ~options ~filename text in
  let statuses =
    List.filter_map begin fun line_num ->
      if line_num < 0 || line_num >= Array.length lines then None else
        let sf = format_at_line format_changes line_num in
        let line = lines.(line_num) in
        let free_like = match line_kind sf with
          | Free_like _ -> true
          | Indicator_at _ -> false
        in
        match line_status ~tab_stops ~mf_comments sf line with
        | Blank -> None
        | status -> Some (line_num, line, free_like, status)
    end (selected_lines ranges)
  in
  let all_comments =
    statuses <> [] &&
    List.for_all (function (_, _, _, Comment _) -> true | _ -> false) statuses
  in
  let text_edit = text_edit ~position_encoding in
  if all_comments then
    List.filter_map begin function
      | line_num, line, _, Comment { uncomment } ->
          Some (text_edit ~line_num line uncomment)
      | _ ->
          None
    end statuses
  else
    let indent =                    (* where [*>] goes in free-like formats *)
      List.fold_left begin fun acc (_, line, free_like, _) ->
        match first_non_blank line with
        | Some i when free_like -> min acc i
        | _ -> acc
      end max_int statuses
    in
    List.filter_map begin function
      | line_num, line, _, Code { comment = Some edit } ->
          Some (text_edit ~line_num line edit)
      | line_num, line, true, (Code _ | Comment _) ->
          Some (text_edit ~line_num line (insert ~at:indent "*> "))
      | _, _, false, (Code { comment = None } | Comment _) | _, _, _, Blank ->
          None
    end statuses
