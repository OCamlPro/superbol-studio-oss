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
open Cir_logic.Types
open Types

let display_fields ~vm:_ ~advancing (fields: cob_field array) state =
  cob_display S32.zero (if advancing then S32.one else S32.zero)
    ((* Array.map (Field.value ~vm)  *)fields);
  Ok (Continue state)
