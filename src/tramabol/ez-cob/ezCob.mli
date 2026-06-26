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

open Ctypes_static

module V1: sig
  val version: int
  module Types: sig
    type cob_pic_symbol
    type cob_field_type
    type cob_field_flags
    type cob_field_attrs
    type cob_memory =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    type cob_memory_ptr
    type cob_field
    type cob_number_store_flags (* = cob_field_flags *)
    type cob_module
    type cob_global
    type error =
      | Nonzero_status of { call : libcob_call; status : nativeint; }
    and libcob_call =
        Module_init of { module_name : string; }
    exception Error of error
  end
  module Printer: sig
    val pp_libcob_call : Format.formatter -> Types.libcob_call -> unit
    val pp_error : Format.formatter -> Types.error -> unit
  end
  module Module: sig
    val global_init : int -> char ptr ptr -> unit
    val enter : Types.cob_module ptr -> unit
    val leave : Types.cob_module ptr -> unit
    val init :
      name:string ->
      source:string ->
      Types.cob_module ptr * Types.cob_global ptr
  end
  module Field: sig
    val move
      : Types.cob_field ptr
      -> Types.cob_field ptr
      -> unit
    val alloc_memory
      : size:int
      -> Types.cob_memory
    (*   val alloc_pic_symbols : *)
    (*     int -> *)
    (*     Libcob_ctypes.V1.Types.Pic_symbol.tag Ctypes.structure Ctypes.CArray.t *)
    val alphanum_attrs
      : byte_size:int
      -> Types.cob_field_attrs Ctypes.ptr
    val numeric_attrs
      : digits:int
      -> scale:int
      -> Types.cob_field_attrs Ctypes.ptr
    val alphanum_field_of_string
      : string
      -> Types.cob_field Ctypes.ptr
    val create
      : memory:Types.cob_memory
      -> offset:int
      -> size:int
      -> attrs:Types.cob_field_attrs ptr
      -> Types.cob_field ptr
  end
  module Termio: sig
    val accept
      : Types.cob_field ptr
      -> unit
    val display
      : ?newline:bool
      -> Types.cob_field ptr array -> unit
  end
  module Numeric: sig
    val add
      : ?number_store_flags:Types.cob_number_store_flags
      -> Types.cob_field ptr
      -> Types.cob_field ptr
      -> unit
    val sub
      : ?number_store_flags:Types.cob_number_store_flags
      -> Types.cob_field ptr
      -> Types.cob_field ptr
      -> unit
  end
end
