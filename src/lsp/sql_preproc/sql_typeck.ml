(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Cobol_data.Types

let get_x_info (cu : Cobol_unit.Types.cobol_unit) name_str =
  (* May raise Not_found | Cobol_unit.Qualmap.Ambiguous _ *)
  Cobol_unit.Qualmap.find
    (Cobol_unit.Qual.name
       (Cobol_common.Srcloc.flagit name_str Cobol_common.Srcloc.dummy) )
    cu.unit_data.data_items.named

let get_length cu name =
  try
    let x_info = get_x_info cu name in
    match x_info with
    | Data_field { def = { payload = { field_size; _ }; _ }; _ } ->
      let size = Cobol_data.Memory.(as_bits field_size / 8) in
      (*  Pretty.out "Size of \"%s\" is %u Bytes@." name size; *)
      size
    | _ -> 0
  with
  | Not_found ->
    (*     Pretty.out " \"%s\" not found " name; *)
    0
  | Cobol_unit.Qualmap.Ambiguous _ ->
    (*     Pretty.out " \"%s\" not found. qualname nel lazy_t found" name; *)
    0

(*TODO*)
let get_type _cu _name = 16

let get_scale cu name =
  try
    let x_info = get_x_info cu name in
    match x_info with
    | Data_field { def = { payload = { field_layout; _ }; _ }; _ } -> begin
      match field_layout with
      | Elementary_field { usage = Display picture; _ } -> (
        match picture.category with
        | FixedNum { scale; _ }
        | FloatNum { scale; _ } ->
          scale
        | _ -> 0 )
      | Elementary_field _
      | Struct_field _ ->
        0
    end
    | _ -> 0
  with
  | Not_found ->
    (*     Pretty.out " \"%s\" not found " name; *)
    0
  | Cobol_unit.Qualmap.Ambiguous _ ->
    (*     Pretty.out " \"%s\" not found. qualname nel lazy_t found" name; *)
    0

(*TODO*)
let get_flags _cu _name = 0

(*TODO*)
let get_ind_addr _cu _name = 0

let print_name (cu : Cobol_unit.Types.cobol_unit) =
  let x_info = get_x_info cu "VBFLD" in
  match x_info with
  | Data_field { def = { payload = { field_layout; field_size; _ }; _ }; _ } ->
    Pretty.out "Size of VBFLD is %u Bytes@."
      Cobol_data.Memory.(as_bits field_size / 8);
    begin
      match field_layout with
      | Elementary_field { usage = Display picture; _ } -> (
        Pretty.out "PIC is %a@." Cobol_data.Picture.pp picture;
        match picture.category with
        | FixedNum { digits = _; scale = _; with_sign = _; _ } -> ()
        | _ -> () )
      | Elementary_field _
      | Struct_field _ ->
        ()
    end
  | _ -> ()
