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

(** Defines some dummy nodes that can be used to fill in missing parts of the
    parse tree. *)

open PTree_types
open Cobol_common.Srcloc.INFIX

let integer_zero = "0"
and integer_one = "1"
and alphanum__ = "_"

let dummy_string ~pos = alphanum__ &@ Cobol_common.Srcloc.raw (pos, pos)

let dummy_name ~pos = dummy_string ~pos

let dummy_name' ~pos : name_or_literal = Name (dummy_string ~pos)

let dummy_qualname ~pos : qualname =
  Name (dummy_name ~pos)

let dummy_qualname' ~pos : qualname with_loc =
  dummy_qualname ~pos &@ Cobol_common.Srcloc.raw (pos, pos)

let dummy_qualident ~pos =
  {
    ident_name = dummy_qualname' ~pos;
    ident_subscripts = [];
  }

let dummy_ident ~pos =
  QualIdent (dummy_qualident ~pos)

let dummy_literal =
  Integer integer_zero

let dummy_alphanum =
  {
    str = alphanum__;
    quotation = Double_quote;
    hexadecimal = false;
    runtime_repr = Native_bytes;
  }

let dummy_expr =
  Atom (Fig Zero)

let dummy_picture ~pos =
  {
    picture_string = "X" &@ Cobol_common.Srcloc.raw (pos, pos);
    picture_locale = None;
    picture_depending = None;
  }

let dummy_picture_locale =
  {
    locale_name = None;
    locale_size = integer_zero;
  }

(* --- *)

let fixed_zero =
  {
    fixed_integral = integer_zero;
    fixed_fractional = integer_one;
  }

let floating_zero =
  {
    float_significand = fixed_zero;
    float_exponent = integer_one;
  }

let boolean_zero =
  {
    bool_base = `Bool;
    bool_value = integer_zero;
  }

(* --- *)

let is_dummy_name name =
  ~&name == alphanum__

let is_dummy_qualname = function
  | Name name -> is_dummy_name name
  | _ -> false

let strip_dummies_from_qualname =
  let rec aux: (Terms.qualname as 'a) -> 'a = function
    | Name _ as qn -> qn
    | Qual (n, qn) when is_dummy_name n -> aux qn
    | Qual (n, qn) when is_dummy_qualname qn -> Name n
    | Qual (n, qn) -> Qual (n, aux qn)
  in
  aux
