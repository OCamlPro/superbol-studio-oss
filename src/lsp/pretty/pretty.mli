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

(* Documentation comments are in the corresponding ML file *)

module Types: sig
  type simple = (unit, Format.formatter, unit) format
  type ('a, 'b) func = ('a, Format.formatter, unit, 'b) format4 -> 'a
  type 'a proc = ('a, Format.formatter, unit) format -> 'a
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a formatting = Format.formatter -> 'a
  type delayed = unit formatting
end

include module type of Types

val print: Format.formatter -> 'a proc
val out: 'a proc
val error: 'a proc
val delayed: ('a, delayed) func
val delayed_to: (delayed -> 'b) -> ('a, 'b) func
val pp_set_margin: Format.formatter -> int -> unit
val blast_margin: Format.formatter -> unit
val to_string: ('a, string) func
val string_to: (string -> 'b) -> ('a, 'b) func
val failwith: ('a, 'b) func
val invalid_arg: ('a, 'b) func
val styles: Fmt.style list -> 'a printer -> 'a printer

val char: char printer
val string: string printer
val text: string printer
val int64: int64 printer
val option: 'a printer -> 'a option printer
val list
  : ?fopen:simple -> ?fsep:simple -> ?fclose:simple -> ?fempty:simple
  -> 'a printer -> 'a list printer
val stack: 'a printer -> 'a list printer
val path: string list printer
val vfield
  : ?label: string printer -> ?sep: unit printer
  -> string -> ('a -> 'b) -> 'b printer -> 'a printer

module Simple: sig
  val int: int -> simple
  val char: char -> simple
  val string: string -> simple
  val map
    : (string -> string)
    -> ('a, 'b, 'c, 'd, 'd, 'a) format6
    -> ('e, 'f, 'g, 'h, 'h, 'e) format6
end

val init_formatters
  : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
