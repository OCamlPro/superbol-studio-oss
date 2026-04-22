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

open Ctypes

module Types (Foreign_types: Ctypes.TYPE) = struct
  open Foreign_types

  let version = constant "COB_FILE_VERSION" int

  module Pic_symbol = struct
    type tag
    let t: tag structure typ = structure "__cob_pic_symbol"
    let symbol         = field t "symbol" char
    let times_repeated = field t "times_repeated" int
    let () = seal t
  end
  let cob_pic_symbol = typedef Pic_symbol.t "cob_pic_symbol"
  type cob_pic_symbol = Pic_symbol.tag structure

  module Field_type = struct
    let t = ushort                                         (* TODO: enum-like *)
    let numeric  = constant "COB_TYPE_NUMERIC" t
    let alphanum = constant "COB_TYPE_ALPHANUMERIC" t
  end

  module Field_flags = struct
    let t = ushort                                    (* TODO: dedicated view *)
    let none = Unsigned.UShort.zero
    let have_sign     = constant "COB_FLAG_HAVE_SIGN"     t
    let sign_separate = constant "COB_FLAG_SIGN_SEPARATE" t
    let sign_leading  = constant "COB_FLAG_SIGN_LEADING"  t
    let blank_zero    = constant "COB_FLAG_BLANK_ZERO"    t
    let justified     = constant "COB_FLAG_JUSTIFIED"     t
    (* let _ = constant "COB_FLAG_BINARY_SWAP"   ushort *)
    (* let _ = constant "COB_FLAG_REAL_BINARY"   ushort *)
    (* let _ = constant "COB_FLAG_IS_POINTER"    ushort *)
    (* let _ = constant "COB_FLAG_NO_SIGN_NIBBLE"ushort *)
    (* let _ = constant "COB_FLAG_IS_FP"         ushort *)
    (* let _ = constant "COB_FLAG_REAL_SIGN"     ushort *)
    (* let _ = constant "COB_FLAG_BINARY_TRUNC"  ushort *)
    let const   = constant "COB_FLAG_CONSTANT"      t
    let value   = constant "COB_FLAG_VALUE"         t
    let content = constant "COB_FLAG_CONTENT"       t
  end
  let cob_field_flags = Field_flags.t
  type cob_field_flags = Unsigned.UShort.t

  module Field_attrs = struct
    type tag
    let t: tag structure typ = structure "__cob_field_attr"
    let type_  = field t "type" Field_type.t
    let digits = field t "digits" ushort
    let scale  = field t "scale" short
    let flags  = field t "flags" cob_field_flags
    let pic    = field t "pic" (ptr cob_pic_symbol)
    let () = seal t
  end
  let cob_field_attrs = typedef Field_attrs.t "cob_field_attr"
  type cob_field_attrs = Field_attrs.tag structure

  type memory =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let cob_memory_ptr = ptr char
  type cob_memory_ptr = char ptr

  module Field = struct
    type tag
    let t: tag structure typ = structure "__cob_field"
    let size = field t "size" size_t
    let data = field t "data" cob_memory_ptr
    let attr = field t "attr" (ptr cob_field_attrs)
    let () = seal t
  end
  let cob_field = typedef Field.t "cob_field"
  type cob_field = Field.tag structure

  (* --- *)

  module Module = struct
    type tag
    let t: tag structure typ = structure "__cob_module"
    (* leave empty for now (we only use pointers to modules *)
    let () = seal t
  end
  let cob_module = typedef Module.t "cob_module"
  type cob_module = Module.tag structure

  module Global = struct
    type tag
    let t: tag structure typ = structure "__cob_global"
    (* leave empty for now (we only use pointers to globals *)
    let () = seal t
  end
  let cob_global = typedef Global.t "cob_global"
  type cob_global = Global.tag structure

end
