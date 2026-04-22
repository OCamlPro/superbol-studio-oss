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

include Libcob_ctypes.V1.Field

open Libcob_ctypes.V1.Types
open Ctypes
open Unsigned

let alloc_memory ~size : memory =
  Ctypes.bigarray_of_ptr Ctypes.array1 size Bigarray.Char @@
  Ctypes.allocate_n char ~count:size

let alloc_pic_symbols n =
  let array = CArray.make cob_pic_symbol (n + 1) in
  Ctypes.setf (CArray.get array n) Pic_symbol.symbol '\000';
  Ctypes.setf (CArray.get array n) Pic_symbol.times_repeated 0;
  array

let alphanum_attrs ~byte_size : cob_field_attrs ptr =
  let pic_symbols = alloc_pic_symbols 1 in
  Ctypes.setf (CArray.get pic_symbols 0) Pic_symbol.symbol 'X';
  Ctypes.setf (CArray.get pic_symbols 0) Pic_symbol.times_repeated byte_size;
  let attrs = Ctypes.make cob_field_attrs in
  Ctypes.setf attrs Field_attrs.type_ Field_type.alphanum;
  Ctypes.setf attrs Field_attrs.digits UShort.zero;
  Ctypes.setf attrs Field_attrs.scale 0;
  Ctypes.setf attrs Field_attrs.flags Field_flags.none;
  Ctypes.setf attrs Field_attrs.pic (CArray.start pic_symbols);
  addr attrs

let numeric_attrs ~digits ~scale : cob_field_attrs ptr =
  let pic_symbols = alloc_pic_symbols 1 in
  Ctypes.setf (CArray.get pic_symbols 0) Pic_symbol.symbol '9';
  Ctypes.setf (CArray.get pic_symbols 0) Pic_symbol.times_repeated digits;
  let attrs = Ctypes.make cob_field_attrs in
  Ctypes.setf attrs Field_attrs.type_ Field_type.numeric;
  Ctypes.setf attrs Field_attrs.digits (UShort.of_int digits);
  Ctypes.setf attrs Field_attrs.scale scale;
  Ctypes.setf attrs Field_attrs.flags Field_flags.none;
  Ctypes.setf attrs Field_attrs.pic (CArray.start pic_symbols);
  addr attrs

let alphanum_field_of_string data_string : cob_field ptr =
  let data = CArray.of_string data_string in
  let length = CArray.length data in
  let field = Ctypes.make cob_field in
  Ctypes.setf field Field.size (Unsigned.Size_t.of_int length);
  Ctypes.setf field Field.data (CArray.start data);
  Ctypes.setf field Field.attr (alphanum_attrs ~byte_size:length);
  addr field

let create ~memory ~offset ~size ~attrs =
  let field = Ctypes.make cob_field in
  Ctypes.setf field Field.size (Unsigned.Size_t.of_int size);
  Ctypes.setf field Field.data (bigarray_start array1 memory +@ offset);
  Ctypes.setf field Field.attr attrs;
  addr field
