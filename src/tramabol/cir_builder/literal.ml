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

open Cobol_data.Types
open Cir_types
open Cobol_common.Srcloc.TYPES
open Types

open Syntax

(* --- *)

let immediate ~builder (lit: literal_value with_loc)
  : (_ immutable_field, _) result =
  match CONST_TABLE.find_opt builder.const_fields ~&lit with
  | Some f ->
      Ok f
  | None ->
      let* f = builder.create_field_from_literal_value lit in
      CONST_TABLE.add builder.const_fields ~&lit f;
      Ok f

let create ~builder (lit: literal_value with_loc) =
  let* f = immediate ~builder lit in
  Ok { field_ref = Constant_field f;
       field_ref_loc = ~@lit }
