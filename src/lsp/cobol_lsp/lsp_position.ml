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

(** This module has utility functions to use {!Lsp.Types.Position} and
    {!Lsp.Types.Range} types with {!Srcloc.lexloc} and {!Srcloc.srcloc}. *)

open Cobol_common
open Srcloc.TYPES
open Lsp.Types

(** Range of length [0], at position [0, 0] *)
let pointwise_range_at_start =
  let start_pos = Position.create ~line:0 ~character:0 in
  Range.create ~start:start_pos ~end_:start_pos

(** {1 Postions {i w.r.t} lexical locations} *)

(** [start_of_lexloc] creates a representation of the start of the given lexical
    location that is suitable for the LSP library. *)
let start_of_lexloc ((start_pos, _end_pos): lexloc) =
  Position.create           (* NOTE: Line numbers start at 0 in LSP protocol. *)
    ~line:(start_pos.pos_lnum - 1)
    ~character:(start_pos.pos_cnum - start_pos.pos_bol)

(** [end_of_lexloc] creates a representation of the end of the given lexical
    location that is suitable for the LSP library. *)
let end_of_lexloc ((_start_pos, end_pos): lexloc) =
  Position.create           (* NOTE: Line numbers start at 0 in LSP protocol. *)
    ~line:(end_pos.pos_lnum - 1)
    ~character:(end_pos.pos_cnum - end_pos.pos_bol)

(** [range_of_lexloc] creates a representation of the given lexical location
    that is suitable for the LSP library. *)
let range_of_lexloc lexloc =
  Range.create ~start:(start_of_lexloc lexloc) ~end_:(end_of_lexloc lexloc)

(** [is_before_lexloc pos lexloc] holds when [pos] strictly precedes [lexloc] *)
let is_before_lexloc (pos: Position.t) lexloc =
  let Position.{ line; character } = start_of_lexloc lexloc in
  pos.line < line ||
  pos.line = line && pos.character < character

(** [is_after_lexloc pos lexloc] holds when [pos] strictly follows [lexloc] *)
let is_after_lexloc (pos: Position.t) lexloc =
  let Position.{ line; character } = end_of_lexloc lexloc in
  pos.line > line ||
  pos.line = line && pos.character > character

(** [is_in_lexloc pos lexloc] holds when [pos] is strictly neither before nor
    after [lexloc] *)
let is_in_lexloc pos lexloc =
  not (is_before_lexloc pos lexloc || is_after_lexloc pos lexloc)

(** [contains_lexloc range lexloc] holds when the range described by [lexloc] is
    strictly contained within [range]. *)
let contains_lexloc Range.{ start; end_ } lexloc =
  is_before_lexloc start lexloc && is_after_lexloc end_ lexloc

(** [intersects_lexloc range lexloc] holds when the range described by [lexloc]
    and [range] have a non-empty intersection. *)
let intersects_lexloc (Range.{ start; end_ } as range) lexloc =
  is_in_lexloc start lexloc ||
  is_in_lexloc end_ lexloc ||
  contains_lexloc range lexloc

(* --- *)

(** {1 Postions {i w.r.t} generalized source locations} *)

(** [range_of_srcloc_in ~filename srcloc] is [range_of_srcloc (Srcloc.lexloc_in
    ~filename srcloc)] *)
let range_of_srcloc_in ~filename srcloc =
  range_of_lexloc (Srcloc.lexloc_in ~filename srcloc)

(** [is_in_srcloc ~filename pos srcloc] is a shorthand for [is_in_lexloc pos
    (Srcloc.lexloc_in ~filename srcloc)] *)
let is_in_srcloc ~filename pos srcloc =
  is_in_lexloc pos (Srcloc.lexloc_in ~filename srcloc)

(* --- *)

class ['x] sieve ~filename ~pos = object
  method fold': 'n. ('n with_loc, 'x) Cobol_common.Visitor.fold =
    fun { loc; _ } ->
    if is_in_srcloc ~filename pos loc
    then Visitor.do_children
    else Visitor.skip_children
end
