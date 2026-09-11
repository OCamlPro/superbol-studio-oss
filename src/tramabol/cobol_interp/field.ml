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
open Cobol_data.Picture.TYPES
open Cobol_data.Types
open Cir_types
open Cir_builder.Types
open Types

open Cir_logic.Syntax
open Cir_logic.Syntax.INFIX                                            (* >>= *)

(* --- *)

let cptr_of_array: cob_field array -> cob_field cptr = fun array ->
  let a = CArray.create (CPtr (CComp CobField.kind)) (Array.length array) in
  Array.iteri (CArray.set a) array;
  CArray.to_ptr a

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

let sign_flags (sign: Cobol_data.Types.display_sign) =
  let with_sign = CobFieldFlag.(to_int @@ enc COB_FLAG_HAVE_SIGN) in
  match sign with
  | Display_unsigned ->
      0
  | Display_signed { sign_position = Leading; sign_separate = false } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING)
  | Display_signed { sign_position = Leading; sign_separate = true } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING) lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_SEPARATE)
  | Display_signed { sign_position = Trailing; sign_separate = false } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING)
  | Display_signed { sign_position = Trailing; sign_separate = true } ->
      with_sign lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_LEADING) lor
      CobFieldFlag.(to_int @@ enc COB_FLAG_SIGN_SEPARATE)

let binaryint_attrs ~constant ~digits ~scale ~sign ~pic =
  let type_ = CobFieldType.COB_TYPE_NUMERIC_BINARY
  and flags =
    (if sign
     then CobFieldFlag.(to_int @@ enc COB_FLAG_HAVE_SIGN)
     else 0)
    lor
    (if constant
     then CobFieldFlag.(to_int @@ enc COB_FLAG_CONSTANT)
     else 0)
  in
  let* digits = Status.lift_ezlibcob_build_error @@ U16.of_int digits
  and* scale  = Status.lift_ezlibcob_build_error @@ S16.of_int scale
  and* flags  = Status.lift_ezlibcob_build_error @@ U16.of_int flags in
  Ok (CobFieldAttr.create ~digits ~scale ~flags
        ~type_:(CobFieldType.(to_u16 (enc type_)))
        ~pic:(pic_symbols pic))

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
  let* digits = Status.lift_ezlibcob_build_error @@ U16.of_int digits
  and* scale  = Status.lift_ezlibcob_build_error @@ S16.of_int scale
  and* flags  = Status.lift_ezlibcob_build_error @@ U16.of_int flags in
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
  | Alphanumeric { picture = { category = Alphabetic _ |
                                          Alphanumeric _; _ } as pic; _ } ->
      Some (Ok (alphanum_attrs ~constant:false ~pic))
  | Display_numeric { picture = { category = FixedNum { digits; scale; _ }; _ }
                        as pic; sign } ->
      Some (fixednum_attrs ~constant:false ~digits ~scale ~sign ~pic)
  | _ ->                                                  (* not supported yet *)
      None

let binary_field ~digits ~sign ~byte_size data_ptr =
  let pic = Cobol_data.Picture.fixed_numeric ~sign digits 0 in
  let data = CPtr.cast UInt8 data_ptr in
  let* attr = binaryint_attrs ~constant:true ~pic ~digits ~scale:0 ~sign in
  Ok (CobField.create ~attr ~data ~size:(U64.of_int_unsafe byte_size))

let int32_field ~digits i =
  binary_field ~digits ~sign:(i < 0) ~byte_size:4 @@
  CPtr.create ~default:(S32.of_int_unsafe i) SInt32

let int64_field ~digits i =
  binary_field ~digits ~sign:Int64.(compare i zero < 0) ~byte_size:8 @@
  CPtr.create ~default:(S64.of_int64 i) SInt64

let numeric_display_field ~digits i_str =
  let sign, with_sign =
    if i_str.[0] = '-'
    then Display_signed { sign_position = Leading; sign_separate = true }, true
    else Display_unsigned, false
  in
  let pic = Cobol_data.Picture.fixed_numeric ~sign:with_sign digits 0 in
  let data = CPtr.cast UInt8 @@ CArray.to_ptr @@ CArray.of_string i_str in
  let* attr = fixednum_attrs ~constant:true ~pic ~digits ~scale:0 ~sign
  and* size = Status.lift_ezlibcob_build_error @@ U64.of_int digits in
  Ok (CobField.create ~attr ~data ~size)

let from_literal_value ~options lit =
  match ~&lit with
  | Alphanum_value str ->
      let size = String.length str in
      let data = CPtr.cast UInt8 @@ CArray.to_ptr @@ CArray.of_string str in
      let pic = Cobol_data.Picture.alphanumeric ~size in
      let attr = alphanum_attrs ~constant:true ~pic in
      let size = U64.of_int_unsafe size in
      Ok (CobField.create ~attr ~data ~size)
  | Integer_value i ->
      let binary = options.integer_literals = `binary_when_small_enough in
      let i_str = Cobol_data.Value.string_of_integer i in     (* decimal repr *)
      let neg = Z.sign i < 0 in
      let digits = String.length i_str - if neg then 1 else 0 in
      if binary && Z.fits_int32 i                              (* BINARY/int32 *)
      then int32_field ~digits (Z.to_int i)
      else if binary && Z.fits_int64 i                         (* BINARY/int64 *)
      then int64_field ~digits (Z.to_int64 i)
      else numeric_display_field ~digits i_str                     (* DISPLAY *)
  | _ ->
      Status.build_error @@ Unsupported { stuff = Literal ~&lit; loc = ~@lit }

let memory_bytes size =
  Cobol_data.Memory.as_bytes size
    ~memory_config:Cobol_data.Memory.amd64_memory_config

let from_definition field_definition (record: cob_record_handle) =
  let field_attrs =
    match ~&field_definition.field_layout with
    | Elementary_field { usage; init_value = _ } ->
        elementary_field_attrs usage
    | Struct_field _ ->
        Some (Ok group_attrs)
  in
  match field_attrs with
  | None ->
      Status.build_error @@ Unsupported { stuff = Field_usage;
                                          loc = ~@field_definition }
  | Some attr ->
      let field_offset = memory_bytes ~&field_definition.field_offset
      and field_size = memory_bytes ~&field_definition.field_size in
      let* attr
      and* size =
        Status.lift_ezlibcob_build_error (U64.of_int field_size)
          ~loc:~@field_definition
      in
      Status.ok @@              (* TODO: check sizes against record data size *)
      CobField.create ~attr ~size
        ~data:(CPtr.add record.record_memory.record_data_ptr field_offset)

(** Internal access *)

let field_size f =
  Status.lift_ezlibcob_runtime_error @@ U64.to_int @@ CobField.get_size f

let is_numeric f =
  S32.(compare (cob_is_numeric f) zero) = 0

let as_int ?loc f =
  if is_numeric f then
    (* TODO: some of those invalid types can easily be detected during typeck or
       CIR construction. *)
    Status.runtime_error ?loc
      (Invalid_field_type { got = f; expected_descr = "integer" })
  else
    Status.lift_ezlibcob_runtime_error ?loc @@ S32.to_int @@ cob_get_int f

(* Fills data_array with the contents of its first cell *)
let fill_carray ~data_array ~data_size ~cell_size =
  let rec aux initialized_size =
    if initialized_size < data_size then
      let todo_size = min initialized_size (data_size - initialized_size) in
      CArray.blit data_array 0 data_array initialized_size todo_size;
      aux (initialized_size + todo_size)
  in
  aux cell_size

(** Operations *)

let rec access_field ~vm : cob_field_handle -> (cob_field, _) result = function
  | Constant_field f ->
      Ok f
  | Field_in_memory f ->
      access_resolved_field ~vm f.field

and access_resolved_field ~vm : cob_field resolved_field -> _ = function
  | Fixed_field f ->
      Ok f
  | Table_field c ->
      access_table_cell ~vm c

and access_table_cell ~vm (c: cob_field resolved_table_cell) =
  let* i = access_int_field_ref ~vm c.cell_index_field in
  if i <= 0 || i > c.cell_index_max
  then
    Status.runtime_error ~loc:c.cell_index_field.field_ref_loc @@
    Table_index_out_of_bounds { index_given = i; index_min = 1;
                                index_max = c.cell_index_max }
  else
    let* r = access_resolved_field ~vm c.cell_first_field in
    let* cell_size = field_size r in
    let data = CobField.get_data r
    and attr = CobField.get_attr r
    and size = CobField.get_size r in
    let offset = (i - 1) * cell_size * c.cell_stride in
    (* TODO: tag as temporary so we can free the structure or manage a pool of
       fields. *)
    (* TODO: clone of a field? *)
    Ok (CobField.create ~attr ~size ~data:(CPtr.add data offset))

and access_int_field_ref ~vm f =
  access_field ~vm f.field_ref >>= as_int ~loc:f.field_ref_loc

let access_field_reference ~vm (f: _ field_reference) state
  : (state * cob_field, _) result =
  let* f = access_field ~vm f.field_ref in
  Ok (state, f)

let rec access_data_reference ~vm (d: _ data_reference) state
  : (state * cob_field, _) result =
  let* f = access_field ~vm d.data_field in
  match d.data_refmod with
  | None ->
      Ok (state, f)
  | Some refmod ->
      let* r = apply_refmod ~vm f refmod in
      Ok (state, r)

and apply_refmod ~vm f { refmod_left; refmod_length } =
  let* offset = access_int_field_ref ~vm refmod_left in
  let* data_size = field_size f in
  if offset <= 0 || offset > data_size + 1 then
    Status.runtime_error ~loc:refmod_left.field_ref_loc @@
    Invalid_refmod { what = `offset; got = offset;
                     expected_max = data_size + 1 }
  else
    let data_array = CArray.of_ptr data_size @@ CobField.get_data f in
    let result_offset = min data_size (offset - 1) in  (* may data_size be 0? *)
    let* result_size =
      match refmod_length with
      | None ->
          Ok (data_size - result_offset)
      | Some refmod_length ->
          let* length = access_int_field_ref ~vm refmod_length in
          let expected_max = data_size - result_offset in
          if length <= 0 || length > expected_max then
            Status.runtime_error ~loc:refmod_length.field_ref_loc @@
            Invalid_refmod { what = `length offset; got = length;
                             expected_max }
          else
            Ok (min (data_size - result_offset) length)
    in
    let result_data = CArray.get_ptr data_array result_offset in
    let pic = Cobol_data.Picture.alphanumeric ~size:result_size in
    let attr = alphanum_attrs ~constant:true ~pic in
    let size = U64.of_int_unsafe result_size in
    Ok (CobField.create ~attr ~data:result_data ~size)

let indirect_field_accessible_data_size base_field ranges =
  let* cell_size = field_size base_field in
  let data_size =
    List.fold_left begin fun x -> function
      | Fixed_range { max }
      | Depending_range { max; _ } -> x * max  (* TODO check init rule for ODO *)
    end cell_size ranges
  in
  Ok (cell_size, data_size)

let init_value: cob_field_access -> _ = fun f ->
  f.access_field.fixed_field_info.field_initial_value

let init ~vm:_ (f: cob_field_access) () : (state, _) result =
  match init_value f with
  | None ->
      Ok ()
  | Some value ->
      let fixed_field = f.access_field.fixed_field in
      cob_move value fixed_field;
      match f.access_ranges with
      | [] ->
          Ok ()
      | ranges ->
          let* cell_size, data_size =
            indirect_field_accessible_data_size fixed_field ranges
          in
          let data_array = CArray.of_ptr data_size @@ CobField.get_data fixed_field in
          fill_carray ~cell_size ~data_size ~data_array;
          Ok ()
