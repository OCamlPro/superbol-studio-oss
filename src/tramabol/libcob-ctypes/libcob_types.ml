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
    type enum =
      | UNKNOWN
      | GROUP
      | BOOLEAN
      | NUMERIC
      | NUMERIC_DISPLAY
      | NUMERIC_BINARY
      | NUMERIC_PACKED
      | NUMERIC_FLOAT
      | NUMERIC_DOUBLE
      | NUMERIC_L_DOUBLE
      | NUMERIC_FP_DEC64
      | NUMERIC_FP_DEC128
      | NUMERIC_FP_BIN32
      | NUMERIC_FP_BIN64
      | NUMERIC_FP_BIN128
      | NUMERIC_COMP5
      | NUMERIC_EDITED
      | ALNUM
      | ALPHANUMERIC
      | ALPHANUMERIC_ALL
      | ALPHANUMERIC_EDITED
      | NATIONAL
      | NATIONAL_EDITED
    let t =
      (* Note: actually stored as an unsigned short instead of an int64_t. We
         define `ml_cob_field_type` in `cobaux/cobaux_typing_helper.h` as an
         enum overlay over these pre-processor constants. *)
      enum "ml_cob_field_type" [
        UNKNOWN            , constant "COB_TYPE_UNKNOWN"             int64_t;
        GROUP              , constant "COB_TYPE_GROUP"               int64_t;
        BOOLEAN            , constant "COB_TYPE_BOOLEAN"             int64_t;
        NUMERIC            , constant "COB_TYPE_NUMERIC"             int64_t;
        NUMERIC_DISPLAY    , constant "COB_TYPE_NUMERIC_DISPLAY"     int64_t;
        NUMERIC_BINARY     , constant "COB_TYPE_NUMERIC_BINARY"      int64_t;
        NUMERIC_PACKED     , constant "COB_TYPE_NUMERIC_PACKED"      int64_t;
        NUMERIC_FLOAT      , constant "COB_TYPE_NUMERIC_FLOAT"       int64_t;
        NUMERIC_DOUBLE     , constant "COB_TYPE_NUMERIC_DOUBLE"      int64_t;
        NUMERIC_L_DOUBLE   , constant "COB_TYPE_NUMERIC_L_DOUBLE"    int64_t;
        NUMERIC_FP_DEC64   , constant "COB_TYPE_NUMERIC_FP_DEC64"    int64_t;
        NUMERIC_FP_DEC128  , constant "COB_TYPE_NUMERIC_FP_DEC128"   int64_t;
        NUMERIC_FP_BIN32   , constant "COB_TYPE_NUMERIC_FP_BIN32"    int64_t;
        NUMERIC_FP_BIN64   , constant "COB_TYPE_NUMERIC_FP_BIN64"    int64_t;
        NUMERIC_FP_BIN128  , constant "COB_TYPE_NUMERIC_FP_BIN128"   int64_t;
        NUMERIC_COMP5      , constant "COB_TYPE_NUMERIC_COMP5"       int64_t;
        NUMERIC_EDITED     , constant "COB_TYPE_NUMERIC_EDITED"      int64_t;
        ALNUM              , constant "COB_TYPE_ALNUM"               int64_t;
        ALPHANUMERIC       , constant "COB_TYPE_ALPHANUMERIC"        int64_t;
        ALPHANUMERIC_ALL   , constant "COB_TYPE_ALPHANUMERIC_ALL"    int64_t;
        ALPHANUMERIC_EDITED, constant "COB_TYPE_ALPHANUMERIC_EDITED" int64_t;
        NATIONAL           , constant "COB_TYPE_NATIONAL"            int64_t;
        NATIONAL_EDITED    , constant "COB_TYPE_NATIONAL_EDITED"     int64_t;
      ]
  end

  module Field_flags = struct
    let t = ushort                                    (* TODO: dedicated view *)
    let none = Unsigned.UShort.zero
    let have_sign      = constant "COB_FLAG_HAVE_SIGN"      t
    let sign_separate  = constant "COB_FLAG_SIGN_SEPARATE"  t
    let sign_leading   = constant "COB_FLAG_SIGN_LEADING"   t
    let blank_zero     = constant "COB_FLAG_BLANK_ZERO"     t
    let justified      = constant "COB_FLAG_JUSTIFIED"      t
    let binary_swap    = constant "COB_FLAG_BINARY_SWAP"    t
    let real_binary    = constant "COB_FLAG_REAL_BINARY"    t
    let is_pointer     = constant "COB_FLAG_IS_POINTER"     t
    let no_sign_nibble = constant "COB_FLAG_NO_SIGN_NIBBLE" t
    let is_fp          = constant "COB_FLAG_IS_FP"          t
    let real_sign      = constant "COB_FLAG_REAL_SIGN"      t
    let binary_trunc   = constant "COB_FLAG_BINARY_TRUNC"   t
    let const          = constant "COB_FLAG_CONSTANT"       t
    let value          = constant "COB_FLAG_VALUE"          t
    let content        = constant "COB_FLAG_CONTENT"        t
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

  module Number_store_flags = struct
    let t = ushort                                    (* TODO: dedicated view *)
    let none = Unsigned.UShort.zero
    let round               = constant "COB_STORE_ROUND"               t
    let keep_on_overflow    = constant "COB_STORE_KEEP_ON_OVERFLOW"    t
    let trunc_on_overflow   = constant "COB_STORE_TRUNC_ON_OVERFLOW"   t
    let away_from_zero      = constant "COB_STORE_AWAY_FROM_ZERO"      t
    let near_away_from_zero = constant "COB_STORE_NEAR_AWAY_FROM_ZERO" t
    let near_even           = constant "COB_STORE_NEAR_EVEN"           t
    let prohibited          = constant "COB_STORE_PROHIBITED"          t
    let toward_greater      = constant "COB_STORE_TOWARD_GREATER"      t
    let toward_lesser       = constant "COB_STORE_TOWARD_LESSER"       t
    let truncation          = constant "COB_STORE_TRUNCATION"          t
    let no_size_error       = constant "COB_STORE_NO_SIZE_ERROR"       t
  end
  let cob_number_store_flags = Number_store_flags.t
  type cob_number_store_flags = Unsigned.UShort.t

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
