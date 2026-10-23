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

let dummy_string ~loc = alphanum__ &@ loc

let dummy_name ~loc = dummy_string ~loc

let dummy_name_or_literal ~loc : name_or_literal = Name (dummy_string ~loc)

let dummy_qualname ~loc : qualname =
  Name (dummy_name ~loc)

let dummy_qualname' ~loc : qualname with_loc =
  dummy_qualname ~loc &@ loc

let dummy_qualident ~loc =
  {
    ident_name = dummy_qualname' ~loc;
    ident_subscripts = [];
  }

let dummy_ident ~loc =
  QualIdent (dummy_qualident ~loc)

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

(* The recovered condition needs to be self-contained: a bare
   [CondObjectOrExpr] would take part in the surrounding abbreviation, and
   report a spurious missing subject or relational operator on top of the
   syntax error that triggered the recovery. *)
let dummy_cond ~loc =
  CondSubject (dummy_expr &@ loc,
               CondRelOp (Eq &@ loc,
                          CondObjectOrExpr (dummy_expr &@ loc) &@ loc) &@ loc)

let dummy_picture ~loc =
  {
    picture_string = "X" &@ loc;
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
    bool_string = integer_zero;
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
