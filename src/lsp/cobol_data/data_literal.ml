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

(** Representation of COBOL literals *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

type alphanum =
  string
  [@@deriving show]

type boolean =
  {
    bool_literal: Cobol_ptree.boolean;
    bool_value: bool array;                          (* Please do not mutate! *)
  }
  [@@deriving show]

let error diags e = Data_diagnostics.add_error e diags

let boolean
    ?(max_length = 8_191)                         (* as per ISO/IEC 1989:2014 *)
    Cobol_ptree.{ payload = { bool_base;
                              bool_value = literal_string } as bool_literal;
                  loc } =
  let diags = Data_diagnostics.none in
  let len = String.length literal_string in
  let diags =
    if len > max_length
    then error diags @@ Overlong_literal { loc; literal_string; max_length }
    else diags
  in
  match bool_base with
  | `Bool ->
      let bool_value = Array.make len false in
      let _, diags =
        Cobol_common.Tokenizing.fold_tokens (literal_string &@ loc) (0, diags)
          ~tokenizer:(fun ~loc:_ -> Data_literal_lexer.boolean)
          ~until:(function Boolean_done -> true | _ -> false)
          ~f:begin fun { payload = b; loc } (i, diags) ->
            let diags =
              match b with
              | Boolean b ->
                  bool_value.(i) <- b;
                  diags
              | Boolean_invalid c ->
                  error diags @@
                  Invalid { loc; stuff = Character_in_boolean_literal c }
              | Boolean_done ->
                  diags
            in
            succ i, diags
          end
      in
      Data_diagnostics.result ~diags ({ bool_literal; bool_value } &@ loc)
  | `Hex ->
      let bool_value = Array.make (len * 4) false in            (* TODO: init *)
      Data_diagnostics.result ~diags ({ bool_literal; bool_value } &@ loc)
