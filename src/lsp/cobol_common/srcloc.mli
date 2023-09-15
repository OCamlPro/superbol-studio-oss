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

module TYPES: sig
  type lexloc = Lexing.position * Lexing.position
  type srcloc
  type copylocs
  type 'a with_loc = { payload: 'a; loc: srcloc; }                [@@deriving ord]
end
type lexloc = TYPES.lexloc
type srcloc = TYPES.srcloc
type copylocs = TYPES.copylocs
type 'a with_loc = 'a TYPES.with_loc =
  { payload: 'a; loc: srcloc; }                                  [@@deriving ord]

module INFIX: sig
  (* Meaning of letters:
     * '~' means projection
     * '&' means payload
     * '@' means location
     * '?' means map-option
  *)
  val ( &@ ): 'a -> srcloc -> 'a with_loc
  val ( &@<- ): 'a -> 'b with_loc -> 'a with_loc
  val ( ~& ): 'a with_loc -> 'a
  val ( ~&? ): 'a with_loc option -> 'a option
  val ( ~@ ): 'a with_loc -> srcloc
  val ( ~@? ): 'a with_loc option -> srcloc option
  val ( ~&@ ): 'a with_loc -> 'a * srcloc
end

val pp_srcloc: srcloc Pretty.printer
val pp_srcloc_struct: srcloc Pretty.printer
val pp_file_loc: srcloc Pretty.printer
val raw
  : ?in_area_a:bool
  -> lexloc
  -> srcloc
val copy
  : filename:string
  -> copyloc:srcloc
  -> srcloc
  -> srcloc
val replacement
  : old:srcloc
  -> new_: srcloc
  -> in_area_a: bool
  -> replloc: srcloc
  -> srcloc

val forget_preproc
  : favor_direction:[`Left | `Right]
  -> traverse_copies:bool
  -> traverse_replaces:bool
  -> srcloc
  -> lexloc
val as_lexloc
  : srcloc
  -> lexloc
val lexloc_in
  : filename: string
  -> srcloc
  -> lexloc
val shallow_multiline_lexloc_in
  : filename: string
  -> srcloc
  -> lexloc
val shallow_single_line_lexloc_in
  : filename: string
  -> srcloc
  -> lexloc
val shallow_single_line_lexlocs_in
  : ?ignore_invalid_filename: bool
  -> filename: string
  -> srcloc
  -> lexloc list
val as_unique_lexloc
  : srcloc
  -> lexloc option
val as_copy
  : srcloc
  -> string with_loc option

val in_area_a: srcloc -> bool
val start_pos: srcloc -> Lexing.position    (* only suitable for Area A checks *)
val start_pos_in: filename: string -> srcloc -> Lexing.position
val end_pos_in: filename: string -> srcloc -> Lexing.position

val concat: srcloc -> srcloc -> srcloc
val concat_srclocs: srcloc list -> srcloc option
val prefix: int -> srcloc -> srcloc
val suffix: int -> srcloc -> srcloc
val trunc_prefix: int -> srcloc -> srcloc
val trunc_suffix: int -> srcloc -> srcloc
val sub : srcloc -> pos:int -> len:int -> srcloc

val pp: 'a Pretty.printer -> 'a with_loc Pretty.printer
val pp_with_loc: 'a Pretty.printer -> 'a with_loc Pretty.printer
val pp_raw_loc: (string * (int * int) * (int * int)) Pretty.printer
val flagit: 'a -> srcloc -> 'a with_loc
val payload: 'a with_loc -> 'a
val loc: 'a with_loc -> srcloc
val as_pair: 'a with_loc -> 'a * srcloc
val locfrom: 'a -> 'b with_loc -> 'a with_loc
val locmap: ('a -> 'b) -> 'a with_loc -> 'b with_loc

val lift_option: 'a option with_loc -> 'a with_loc option
val lift_result: ('a, 'e) result with_loc -> ('a with_loc, 'e with_loc) result
val concat_locs: _ with_loc list -> srcloc option
val concat_strings_with_loc: string with_loc -> string with_loc -> string with_loc
val copy_from: filename:string -> copyloc:srcloc -> 'a with_loc -> 'a with_loc

val no_copy: copylocs
val new_copy: copyloc:srcloc -> string -> copylocs -> copylocs
val mem_copy: string -> copylocs -> bool
