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

type sign =
  | Signed
  | Unsigned

type int_kind =
  | Int8
  | Int16
  | Int32
  | Int64

type float_kind =
  | Float
  | Double

type type_ =
  | STVoid of { const : bool }
  | STChar of { const : bool; }
  | STInt of { const : bool; kind : int_kind; sign : sign }
  | STFloat of { const : bool; kind : float_kind }
  | STEnum of { const : bool; enum : enum }
  | STComp of { const : bool; comp : comp; unref : bool }
  | STPtr of { const : bool; type_ : type_ }
  | STArray of { const : bool; type_ : type_; size : int } (* only when in a struct *)

and enum = {
    enum_ml_type : string;
    enum_c_type : string;

    (* fields needed for building *)

    enum_module_name : string;
    enum_name : string;
    enum_items : (string * int) list;
  }

and comp = {
    comp_ml_type : string;
    comp_c_type : string;

    (* fields needed for building *)

    comp_module_name : string;
    comp_name : string;
    mutable comp_fields : (string * type_) list;

    comp_no_constr : bool;
    comp_no_destr : bool;

    comp_struct : bool;
  }

type arg_kind =
  | Normal
  | Constant of string
  | VASize
  | VAArg

type arg_name = {
    name: string;
    default : bool;
  }

type arg = {
    arg_name : arg_name;
    arg_type : type_;
    arg_kind : arg_kind;
  }

type ret = {
    ret_type : type_;
  }

type va_arg =
  | VAUnroll of { limit : int; varg : arg }
  | VAFormat

type stub = {
    fun_name : string; (* binding function name *)
    c_fun_name : string; (* bound function name *)
    args : arg list; (* the function arguments *)
    va_args : va_arg option; (* how to deal with var args *)
    ret : ret; (* the function return value *)
  }

type contents = {
    stubs : stub list;
    enums : enum list;
    comps : comp list;
  }
