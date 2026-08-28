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
open Cobol_ir.Types

let display_fields ~vm ~advancing (fields: Types.cob_field_handle array) =
  cob_display S32.zero (if advancing then S32.one else S32.zero)
    (Field.values ~vm fields);
  Ok ()
