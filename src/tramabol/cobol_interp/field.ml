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
open Cobol_data.Types
open Cobol_ir.Types
open Types

open Cobol_ir.Syntax

(* --- *)

let errors e = Error e
let error e = errors (NEL.one e)
let lift_ezlibcob_error = function
  | Ok _ as x -> x
  | Error e -> error (Ezlibcob_error e)


(* --- *)

let cptr_of_array: cob_field array -> cob_field cptr = fun array ->
  let a = CArray.create (CPtr (CComp CobField.kind)) (Array.length array) in
  Array.iteri (CArray.set a) array;
  CArray.to_ptr a

let value ~vm:_ : cob_field_handle -> cob_field = function
  | Field_constant f -> f
  | Field_in_memory { field_value = Fixed_field f; _ } -> f

let values ~vm : cob_field_handle array -> cob_field array = fun fields ->
  Array.map (value ~vm) fields

(* --- *)

let pic_symbols (pic: picture) =       (* TODO: pass env for special symbols? *)
  let pic_len = List.length pic.pic in
  let array = CArray.create (CComp CobPicSymbol.kind) (pic_len + 1) in
  List.iteri begin fun i Cobol_data.Picture.TYPES.{ symbol; symbol_occurences } ->
    let p = CArray.get_ptr array i in
    CobPicSymbol.set_symbol p (Cobol_data.Picture.char_of_symbol symbol);
    CobPicSymbol.set_times_repeated p (S32.of_int_unsafe symbol_occurences);
  end pic.pic;
  let p = CArray.get_ptr array pic_len in
  CobPicSymbol.set_symbol p '\000';
  CobPicSymbol.set_times_repeated p S32.zero;
  CArray.to_ptr array

let sign_flags (sign: Cobol_data.Picture.TYPES.sign_config option) =
  let with_sign = CobFieldFlag.(to_int @@ enc COB_FLAG_HAVE_SIGN) in
  match sign with
  | None ->
      0
  | Some { sign_position = Leading; sign_separate = false } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING)
  | Some { sign_position = Leading; sign_separate = true } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING) lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_SEPARATE)
  | Some { sign_position = Trailing; sign_separate = false } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING)
  | Some { sign_position = Trailing; sign_separate = true } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING) lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_SEPARATE)

let fixednum_attrs ~constant ~digits ~scale ~sign ~pic =
  let type_ =
    if Cobol_data.Picture.is_edited pic
    then CobFieldType.COB_TYPE_NUMERIC_EDITED
    else CobFieldType.COB_TYPE_NUMERIC_DISPLAY
  and flags =
    sign_flags sign lor
    if constant
    then CobFieldFlag.(to_int @@ enc COB_FLAG_CONSTANT)
    else 0
  in
  let* digits = lift_ezlibcob_error @@ U16.of_int digits
  and* scale  = lift_ezlibcob_error @@ S16.of_int scale
  and* flags  = lift_ezlibcob_error @@ U16.of_int flags in
  Ok (CobFieldAttr.create ~digits ~scale ~flags
        ~type_:(CobFieldType.(to_u16 (enc type_)))
        ~pic:(pic_symbols pic))

let alphanum_attrs ~constant ~pic =
  let type_ =
    if Cobol_data.Picture.is_edited pic
    then CobFieldType.COB_TYPE_ALPHANUMERIC
    else CobFieldType.COB_TYPE_ALPHANUMERIC_EDITED
  and flags =
    if constant
    then CobFieldFlag.(to_u16 @@ enc COB_FLAG_CONSTANT)
    else U16.zero
  in
  CobFieldAttr.create
    ~type_:CobFieldType.(to_u16 (enc type_))
    ~digits:U16.zero
    ~scale:S16.zero
    ~flags
    ~pic:(pic_symbols pic)

let group_attrs =
  CobFieldAttr.create
    ~type_:(CobFieldType.(to_u16 (enc COB_TYPE_GROUP)))
    ~digits:U16.zero
    ~scale:S16.zero
    ~flags:U16.zero
    ~pic:(CobPicSymbol.null ())

let elementary_field_attrs = function
  | Display ({ category = Alphabetic _ | Alphanumeric _; _ } as pic) ->
      Some (Ok (alphanum_attrs ~constant:false ~pic))
  | Display ({ category = FixedNum { digits; scale; sign; _ }; _ } as pic) ->
      Some (fixednum_attrs ~constant:false ~digits ~scale ~sign ~pic)
  | _ ->                                                  (* not supported yet *)
      None

(* TODO: work on 78-level constants will enable us to use types for literals
   defined in `Cobol_data` instead) *)
let from_literal (lit: Cobol_ptree.literal Cobol_ptree.with_loc) =
  match ~&lit with
  | Alphanum a ->
      let size = String.length a.str in
      let data = CPtr.cast UInt8 @@ CArray.to_ptr @@ CArray.of_string a.str in
      let pic = Cobol_data.Picture.alphanumeric ~size in
      let attr = alphanum_attrs ~constant:true ~pic in
      let* size = lift_ezlibcob_error @@ U64.of_int size in
      Ok (CobField.create ~attr ~data ~size)
  | _ ->
      error @@ Unsupported { stuff = Literal ~&lit; loc = ~@lit }

let memory_bytes size =
  Cobol_data.Memory.as_bytes size
    ~memory_config:Cobol_data.Memory.amd64_memory_config

let in_record_memory field_definition (record: cob_record_handle) =
  let field_attrs, field_init =
    match ~&field_definition.field_layout with
    | Elementary_field { usage; init_value } ->
        elementary_field_attrs usage, init_value
    | Struct_field _ ->
        Some (Ok group_attrs), None
  in
  match field_attrs with
  | None ->
      error @@ Unsupported { stuff = Field_usage; loc = ~@field_definition }
  | Some attr ->
      let field_offset = memory_bytes ~&field_definition.field_offset
      and field_size = memory_bytes ~&field_definition.field_size in
      let* attr
      and* size = lift_ezlibcob_error @@ U64.of_int field_size in
      let cob_field =           (* TODO: check sizes against record data size *)
        CobField.create ~attr ~size
          ~data:(CPtr.add record.record_memory.record_data_ptr field_offset)
      in
      let* field_initial_value =
        match field_init with
        | None ->
            Ok None
        | Some lit ->
            let* f = from_literal lit in
            Ok (Some f)
      in
      Ok {
        field_value = Fixed_field cob_field;
        field_initial_value;
        field_definition;
      }

(** Operations *)

let init ~vm:_ : cob_field_mutable -> (unit, _) result = function
  | { field_initial_value = None; _ } ->
      Ok ()                                  (* Nothing to do? Zeros? Spaces? *)
  | { field_initial_value = Some value;
      field_value = Fixed_field field; _ } ->
      cob_move value field;
      Ok ()

let as_int ~vm:_ : cob_field_handle -> (int, _) result = function
  | Field_constant f
  | Field_in_memory { field_value = Fixed_field f; _ } ->
      lift_ezlibcob_error @@ S32.to_int @@ cob_get_int f
