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

type alphanum =
  {
    str: string;                                              (* given string *)
    quotation: alphanum_quote;
    hexadecimal: bool;
    runtime_repr: alphanum_repr;
  }

and alphanum_quote =
  | Simple_quote (* '...' *)
  | Double_quote (* "..." *)

and alphanum_repr =
  | Native_bytes
  | Null_terminated_bytes

[@@deriving ord]

let alphanum_of_string ?(quotation = Double_quote) ?(hexadecimal = false)
    ?(zero_terminated = false) str =
  {
    str;
    quotation;
    hexadecimal;
    runtime_repr = if zero_terminated then Null_terminated_bytes else Native_bytes;
  }

(** Pretty-prints the given alphanum as a literal. *)
let pp_alphanum ppf { hexadecimal; quotation; str; runtime_repr } =
  if runtime_repr = Null_terminated_bytes then Fmt.char ppf 'Z';
  if hexadecimal then Fmt.char ppf 'X';
  let q = match quotation with Simple_quote -> '\'' | Double_quote -> '"' in
  Fmt.pf ppf "%c%s%c" q str q

type national = string                                             (* for now *)
[@@deriving ord, show]
