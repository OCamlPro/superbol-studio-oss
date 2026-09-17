(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezlibcob.V1
open Cir_types
open Types

open Cir_logic.Syntax

let condition ~vm ((polarity, a, b) : _ condition) () : (state * bool, _) result =
  let* (), a = Field.access_field_reference ~vm a () in
  let* (), b = Field.access_field_reference ~vm b () in
  let res = cob_cmp a b in
  Ok ((), S32.to_int_unsafe res != 0 = polarity)
