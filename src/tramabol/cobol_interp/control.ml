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

open Cir_logic.Types

open Cir_logic.Syntax

let stop ~vm:_ ?status state =
  match status with
  | None ->
      Ok (Stop (state, 0))
  | Some f ->
      let* status = Field.as_int f in
      Ok (Stop (state, status))
