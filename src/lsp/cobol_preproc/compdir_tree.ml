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

open Cobol_common.Srcloc.TYPES
let pp_with_loc = Cobol_common.Srcloc.pp_with_loc

type directive_kind =
  | Define_directive
  | Elif_directive
  | Else_directive
  | EndIf_directive
  | If_directive
  | Set_directive
  | Source_directive

type directive =
  | Lexing of lexing_directive
  | Preproc of preproc_directive

and lexing_directive =
  | Source_format_is_free of srcloc [@printer fun _ _ -> ()]
  | Source_format_is of string with_loc
  | Set_sourceformat of string with_loc

and preproc_directive =
  | Set of set_operand with_loc
  | Define_off of string with_loc
  | Define of definition
  | If of boolexpr with_loc
  | Elif of boolexpr with_loc
  | Else
  | End_if

and definition =
  {
    var: string with_loc;
    expr: define_expr with_loc;
    override: bool;
  }

and define_expr =
  | Alphanum_literal of Cobol_data.Literal.alphanum with_loc
  | Parameter

and boolexpr =
  | Boolean_literal of Cobol_data.Literal.boolean with_loc
  | Defined_condition of
      {
        var: string with_loc;
        polarity: bool;                        (* false for `var NOT DEFINED' *)
      }

and set_operand =
  | Add_srv
  | Add_syn
  | Area_check of bool
  | Assign
  | Bound of bool
  | Call_FH
  | Check_num of bool
  | Comp_1
  | Constant
  | DPC_in_data of bool
  | Fold_copy_name of bool
  | Make_syn
  | Nest_call
  | ODO_slide of bool
  | Remove
  | SP_zero of bool
  | SS_range of bool

[@@deriving show]
