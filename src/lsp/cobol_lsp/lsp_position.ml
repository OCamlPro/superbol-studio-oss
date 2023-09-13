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
let none_range =
  let none_pos = Position.create ~line:0 ~character:0 in
  Range.create ~start:none_pos ~end_:none_pos

(** {1 Postions {i w.r.t} lexical locations} *)

(** [range_of_lexloc] creates a representation of the given lexical location
    that is suitable for the LSP library. *)
let range_of_lexloc ((start_pos, end_pos): lexloc) =
  (* NOTE: Line numbers start at 0 in LSP protocol. *)
  let sl = start_pos.pos_lnum - 1
  and sc = start_pos.pos_cnum - start_pos.pos_bol
  and el = end_pos.pos_lnum - 1
  and ec = end_pos.pos_cnum - end_pos.pos_bol in
  Range.create
    ~start:(Position.create ~line:sl ~character:sc)
    ~end_:(Position.create ~line:el ~character:ec)

(** [is_before_lexloc pos lexloc] holds when [pos] is strictly before [lexloc] *)
let is_before_lexloc pos lexloc =
  let Range.{start = {line; character;}; _} = range_of_lexloc lexloc in
  Position.(pos.line < line || (pos.line = line && pos.character < character))

(** [is_after_lexloc pos lexloc] holds when [pos] is strictly after [lexloc] *)
let is_after_lexloc pos lexloc =
  let Range.{end_ = {line; character;}; _} = range_of_lexloc lexloc in
  Position.(pos.line > line || (pos.line = line && pos.character > character))

(** [is_in_lexloc pos lexloc] holds when [pos] is neither before or after
    [lexloc] *)
let is_in_lexloc pos lexloc =
  (not @@ is_after_lexloc pos lexloc) && (not @@ is_before_lexloc pos lexloc)

(** [contains_lexloc range lexloc] holds when [lexloc] is strictly contained
    inside [range]. *)
let contains_lexloc Range.{start; end_} lexloc =
  is_before_lexloc start lexloc && is_after_lexloc end_ lexloc

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
