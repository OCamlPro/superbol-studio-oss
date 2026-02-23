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
  | End_directive
  | EndIf_directive
  | If_directive
  | Set_directive
  | Source_directive

type directive =
  | Lexing of lexing_directive
  | Preproc of preproc_directive

and lexing_directive =
  | Source_format_is_free of srcloc [@printer fun _ _ -> ()]
  | Source_format_is of Cobol_data.Literal.alphanum with_loc
  | Set_sourceformat of Cobol_data.Literal.alphanum with_loc

and preproc_directive =
  | Set of set_operand with_loc
  | Define_off of var with_loc
  | Define of definition
  | If of boolexpr with_loc
  | Elif of boolexpr with_loc
  | Else
  | End
  | End_if

and definition =
  {
    var: var with_loc;
    value: definition_value with_loc;
    override: bool;
  }

and var = Preproc_env.VAR.t

and definition_value =
  | Literal_definition of literal
  | Parameter_definition

and literal =
  | Alphanum of Cobol_data.Literal.alphanum with_loc
  | Boolean of Cobol_data.Literal.boolean with_loc
  | Numeric of Cobol_data.Literal.fixed with_loc

and term =
  | Variable of var with_loc
  | Literal of literal

and boolexpr =
  | Constant_condition of
      {
        left_operand: term;
        polarity: bool;                      (* false for `negative polarity' *)
        operator: condition_operator;
        right_operand: term;
      }
  | Value_condition of
      {
        var: var with_loc;
        polarity: bool;
      }
  | Defined_condition of
      {
        var: var with_loc;
        polarity: bool;                        (* false for `var NOT DEFINED' *)
      }
  | Set_condition of
      {
        var: var with_loc;
        polarity: bool;                        (* false for `var NOT SET' *)
      }

and condition_operator = Eq | Ge | Gt | Le | Lt | Ne

and set_operand =
  | Add_srv
  | Add_syn
  | ANSI_85 of bool
  | Area_check of bool
  | Assign
  | Bound of bool
  | Call_FH
  | Check_num of bool
  | Comp_1
  | Constant of var with_loc * literal
  | DPC_in_data of bool
  | Fold_copy_name of bool
  | Int_level of Cobol_data.Literal.fixed with_loc
  | Make_syn
  | Nest_call
  | N_symbol of string with_loc
  | ODO_slide of bool
  | Remove
  | Sequential of string with_loc
  | Sign of string with_loc
  | SP_zero of bool
  | SS_range of bool

[@@deriving show]
