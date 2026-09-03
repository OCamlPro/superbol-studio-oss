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

open Ezlibcob.V1

let max_cob_field_value_shown_in_pp = 20

(* --- *)

let pp_int64 ppf f =
  Fmt.fmt "%Ld/int64" ppf @@ S64.to_int64 @@
  CPtr.get @@ CPtr.cast SInt64 @@ CobField.get_data f

let pp_int32 ppf f =
  Fmt.fmt "%d/int32" ppf @@ S32.to_int_unsafe @@
  CPtr.get @@ CPtr.cast SInt32 @@ CobField.get_data f

let pp_cob_field ppf f =
  match
    CobFieldType.(dec @@ of_u16 @@ CobFieldAttr.get_type @@
                  CobField.get_attr f),
    U64.to_int @@ CobField.get_size f
  with
  | COB_TYPE_NUMERIC_BINARY, Ok 4 ->
      pp_int32 ppf f
  | COB_TYPE_NUMERIC_BINARY, Ok 8 ->
      pp_int64 ppf f
  | _ ->
      let size, too_long =
        match U64.to_int @@ CobField.get_size f with
        | Ok s when s <= max_cob_field_value_shown_in_pp ->
            s, false
        | Ok _ | Error IntegerOverflow ->
            max_cob_field_value_shown_in_pp, true
      in
      let data =
        CArray.to_string @@ CArray.of_ptr size @@
        CPtr.cast Char @@ CobField.get_data f
      in
      if too_long
      then Pretty.print ppf "%S(truncated)" data
      else Pretty.print ppf "%S" data
