(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

type alphanum_quote =
  | Simple_quote (* '...' *)
  | Double_quote (* "..." *)
[@@deriving ord]

type alphanum_repr =
  | Native_bytes
  | Null_terminated_bytes
[@@deriving ord]

type alphanum =
  {
    str: string;
    given_str: string;
    quotation: alphanum_quote;
    hexadecimal: bool;
    runtime_repr: alphanum_repr;
  }
[@@deriving ord]

(** Assumes a leading zero in case of odd length. *)
(* CHECKME: this behavior may depend on compiler/dialect. *)
let of_hex hex =
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | _ -> invalid_arg "Invalid hexadecimal digit"
  in
  let len = String.length hex in
  let len, hex = if len mod 2 <> 0 then len + 1, "0"^hex else len, hex in
  String.init (len / 2) begin fun i ->
    let hi = digit hex.[2 * i] in
    let lo = digit hex.[2 * i + 1] in
    Char.chr ((hi lsl 4) lor lo)
  end

let alphanum_of_string ?(quotation = Double_quote) ?(hexadecimal = false)
    ?(zero_terminated = false) given_str =
  {
    str = if hexadecimal then of_hex given_str else given_str;
    given_str;
    quotation;
    hexadecimal;
    runtime_repr = if zero_terminated then Null_terminated_bytes else Native_bytes;
  }

(** Pretty-prints the given alphanum as a literal; appends a slash and an
    escaped "real value" only if the alphanum value was given in hexadeciaml. *)
let pp_alphanum ppf { hexadecimal; quotation; given_str; str; runtime_repr; _ } =
  if runtime_repr = Null_terminated_bytes then Fmt.char ppf 'Z';
  if hexadecimal then Fmt.char ppf 'X';
  let q = match quotation with Simple_quote -> '\'' | Double_quote -> '"' in
  Fmt.pf ppf "%c%s%c" q given_str q;
  if hexadecimal then Fmt.pf ppf "/%S" str

type national = string                                             (* for now *)
[@@deriving ord, show]
