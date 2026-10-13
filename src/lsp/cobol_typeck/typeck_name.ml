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

open Cobol_data.Types
open Cobol_common.Srcloc.TYPES

open Cobol_common.Srcloc.INFIX

(* --- *)

(* TODO: pass program environment config *)
let literal_value_to_string: literal_value -> string = function
  | Alphanum_value x -> x
  | Boolean_value x -> Cobol_data.Value.string_of_boolean x
  | Integer_value x -> Cobol_data.Value.string_of_integer x
  | Fixed_value x -> Cobol_data.Value.string_of_fixed x
  | Floating_value x -> Cobol_data.Value.string_of_floating x
  | Zero_value -> "\x00"
  | Space_value -> " "
  | Quote_value -> "'"                                              (* CHECKME *)
  | Low_value -> "\x00"
  | High_value -> "\xff"
  | All_alphanum_value a -> a                                       (* CHECKME *)

(* --- *)

let of_compilation_unit: Cobol_ptree.compilation_unit -> string with_loc = function
  | Program { program_name = name; _ }
  | Function { function_name = name; _ }
  | ClassDefinition { class_name = name; _ }
  | InterfaceDefinition { interface_name = name; _ } ->
      match
        match ~&name with
        | Name x ->
            Cobol_data.Literal.alphanum_literal_value
              (Cobol_ptree.alphanum_of_string ~&x &@<- x)
        | Alphanum _
        | Boolean _
        | Integer _
        | Fixed _
        | Floating _
        | National _
        | Concat _
        | StrConcat _
        | Fig _
        | NumFig _ as x ->
            Cobol_data.Literal.value (x &@<- name)
      with
      | Ok v ->
          Cobol_common.Srcloc.map_payload literal_value_to_string v
      | Error _ ->                                       (* fallback (for now) *)
          Pretty.to_string "%a" Cobol_ptree.pp_name_or_literal ~&name &@<- name
