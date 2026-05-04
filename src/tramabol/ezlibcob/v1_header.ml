#2 "src/ezlibcob/v1_header.ml"
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

module LIBCOB = struct

  module TYPES = struct

    type ty =
      | COB_TYPE_UNKNOWN
      | COB_TYPE_GROUP
      | COB_TYPE_BOOLEAN
      | COB_TYPE_NUMERIC
      | COB_TYPE_NUMERIC_DISPLAY
      | COB_TYPE_NUMERIC_BINARY
      | COB_TYPE_NUMERIC_PACKED
      | COB_TYPE_NUMERIC_FLOAT
      | COB_TYPE_NUMERIC_DOUBLE
      | COB_TYPE_NUMERIC_L_DOUBLE
      | COB_TYPE_NUMERIC_FP_DEC64
      | COB_TYPE_NUMERIC_FP_DEC128
      | COB_TYPE_NUMERIC_FP_BIN32
      | COB_TYPE_NUMERIC_FP_BIN64
      | COB_TYPE_NUMERIC_FP_BIN128
      | COB_TYPE_NUMERIC_COMP5

      | COB_TYPE_NUMERIC_EDITED

      | COB_TYPE_ALNUM
      | COB_TYPE_ALPHANUMERIC
      | COB_TYPE_ALPHANUMERIC_ALL
      | COB_TYPE_ALPHANUMERIC_EDITED

      | COB_TYPE_NATIONAL
      | COB_TYPE_NATIONAL_EDITED

    type flag =
      | COB_FLAG_HAVE_SIGN
      | COB_FLAG_SIGN_SEPARATE
      | COB_FLAG_SIGN_LEADING
      | COB_FLAG_BLANK_ZERO
      | COB_FLAG_JUSTIFIED
      | COB_FLAG_BINARY_SWAP
      | COB_FLAG_REAL_BINARY
      | COB_FLAG_IS_POINTER
      | COB_FLAG_NO_SIGN_NIBBLE
      | COB_FLAG_IS_FP
      | COB_FLAG_REAL_SIGN
      | COB_FLAG_BINARY_TRUNC
      | COB_FLAG_CONSTANT

    type c_pointer
    type cob_module = {
      ml_module : c_pointer ;
      mutable need_init : bool ;
    }
    type context = {
      ml_context : c_pointer ;
      need_module_init : bool ;
    }
    type buffer = {
      ml_buffer : c_pointer ;
      buf_size : int ;
    }
    type field_attr (* cob_field_attr*, behind an abstract tag *)
    type field (* cob_field*, behind an abstract tag *)
    type decimal = int (* the number of the register containing the decimal *)
    type voids (* void*, behind an abstract tag *)
    type file (* cob_file*, behind an abstract tag *)
  end

  open TYPES

  let int_of_ty ty =
    match ty with
    | COB_TYPE_UNKNOWN		-> 0x00
    | COB_TYPE_GROUP		-> 0x01
    | COB_TYPE_BOOLEAN		-> 0x02
    | COB_TYPE_NUMERIC		-> 0x10
    | COB_TYPE_NUMERIC_DISPLAY	-> 0x10
    | COB_TYPE_NUMERIC_BINARY		-> 0x11
    | COB_TYPE_NUMERIC_PACKED		-> 0x12
    | COB_TYPE_NUMERIC_FLOAT		-> 0x13
    | COB_TYPE_NUMERIC_DOUBLE		-> 0x14
    | COB_TYPE_NUMERIC_L_DOUBLE	-> 0x15
    | COB_TYPE_NUMERIC_FP_DEC64	-> 0x16
    | COB_TYPE_NUMERIC_FP_DEC128	-> 0x17
    | COB_TYPE_NUMERIC_FP_BIN32	-> 0x18
    | COB_TYPE_NUMERIC_FP_BIN64	-> 0x19
    | COB_TYPE_NUMERIC_FP_BIN128	-> 0x1A
    | COB_TYPE_NUMERIC_COMP5		-> 0x1B
    | COB_TYPE_NUMERIC_EDITED		-> 0x24
    | COB_TYPE_ALNUM			-> 0x20
    | COB_TYPE_ALPHANUMERIC		-> 0x21
    | COB_TYPE_ALPHANUMERIC_ALL	-> 0x22
    | COB_TYPE_ALPHANUMERIC_EDITED	-> 0x23
    | COB_TYPE_NATIONAL		-> 0x40
    | COB_TYPE_NATIONAL_EDITED	-> 0x41

  let string_of_ty ty =
    match ty with
    | COB_TYPE_UNKNOWN -> "COB_TYPE_UNKNOWN"
    | COB_TYPE_GROUP -> "COB_TYPE_GROUP"
    | COB_TYPE_BOOLEAN -> "COB_TYPE_BOOLEAN"
    | COB_TYPE_NUMERIC -> "COB_TYPE_NUMERIC"
    | COB_TYPE_NUMERIC_DISPLAY -> "COB_TYPE_NUMERIC_DISPLAY"
    | COB_TYPE_NUMERIC_BINARY -> "COB_TYPE_NUMERIC_BINARY"
    | COB_TYPE_NUMERIC_PACKED -> "COB_TYPE_NUMERIC_PACKED"
    | COB_TYPE_NUMERIC_FLOAT -> "COB_TYPE_NUMERIC_FLOAT"
    | COB_TYPE_NUMERIC_DOUBLE -> "COB_TYPE_NUMERIC_DOUBLE"
    | COB_TYPE_NUMERIC_L_DOUBLE -> "COB_TYPE_NUMERIC_L_DOUBLE"
    | COB_TYPE_NUMERIC_FP_DEC64 -> "COB_TYPE_NUMERIC_FP_DEC64"
    | COB_TYPE_NUMERIC_FP_DEC128 -> "COB_TYPE_NUMERIC_FP_DEC128"
    | COB_TYPE_NUMERIC_FP_BIN32 -> "COB_TYPE_NUMERIC_FP_BIN32"
    | COB_TYPE_NUMERIC_FP_BIN64 -> "COB_TYPE_NUMERIC_FP_BIN64"
    | COB_TYPE_NUMERIC_FP_BIN128 -> "COB_TYPE_NUMERIC_FP_BIN128"
    | COB_TYPE_NUMERIC_COMP5 -> "COB_TYPE_NUMERIC_COMP5"
    | COB_TYPE_NUMERIC_EDITED -> "COB_TYPE_NUMERIC_EDITED"
    | COB_TYPE_ALNUM -> "COB_TYPE_ALNUM"
    | COB_TYPE_ALPHANUMERIC -> "COB_TYPE_ALPHANUMERIC"
    | COB_TYPE_ALPHANUMERIC_ALL -> "COB_TYPE_ALPHANUMERIC_ALL"
    | COB_TYPE_ALPHANUMERIC_EDITED -> "COB_TYPE_ALPHANUMERIC_EDITED"
    | COB_TYPE_NATIONAL -> "COB_TYPE_NATIONAL"
    | COB_TYPE_NATIONAL_EDITED -> "COB_TYPE_NATIONAL_EDITED"

  let ty_of_int ty =
    match ty with
    | 0x00 -> COB_TYPE_UNKNOWN
    | 0x01 -> COB_TYPE_GROUP
    | 0x02 -> COB_TYPE_BOOLEAN
    (*    | 0x10 -> COB_TYPE_NUMERIC *)
    | 0x10 -> COB_TYPE_NUMERIC_DISPLAY
    | 0x11 -> COB_TYPE_NUMERIC_BINARY
    | 0x12 -> COB_TYPE_NUMERIC_PACKED
    | 0x13 -> COB_TYPE_NUMERIC_FLOAT
    | 0x14 -> COB_TYPE_NUMERIC_DOUBLE
    | 0x15 -> COB_TYPE_NUMERIC_L_DOUBLE
    | 0x16 -> COB_TYPE_NUMERIC_FP_DEC64
    | 0x17 -> COB_TYPE_NUMERIC_FP_DEC128
    | 0x18 -> COB_TYPE_NUMERIC_FP_BIN32
    | 0x19 -> COB_TYPE_NUMERIC_FP_BIN64
    | 0x1A -> COB_TYPE_NUMERIC_FP_BIN128
    | 0x1B -> COB_TYPE_NUMERIC_COMP5
    | 0x24 -> COB_TYPE_NUMERIC_EDITED
    | 0x20 -> COB_TYPE_ALNUM
    | 0x21 -> COB_TYPE_ALPHANUMERIC
    | 0x22 -> COB_TYPE_ALPHANUMERIC_ALL
    | 0x23 -> COB_TYPE_ALPHANUMERIC_EDITED
    | 0x40 -> COB_TYPE_NATIONAL
    | 0x41 -> COB_TYPE_NATIONAL_EDITED
    | _ -> assert false

  let bit_of_flag flag =
    match flag with
    | COB_FLAG_HAVE_SIGN -> 0           (* 0x0001 *)
    | COB_FLAG_SIGN_SEPARATE -> 1	(* 0x0002 *)
    | COB_FLAG_SIGN_LEADING -> 2	(* 0x0004 *)
    | COB_FLAG_BLANK_ZERO -> 3        	(* 0x0008 *)
    | COB_FLAG_JUSTIFIED -> 4     	(* 0x0010 *)
    | COB_FLAG_BINARY_SWAP -> 5	        (* 0x0020 *)
    | COB_FLAG_REAL_BINARY -> 6 	(* 0x0040 *)
    | COB_FLAG_IS_POINTER -> 7  	(* 0x0080 *)
    | COB_FLAG_NO_SIGN_NIBBLE -> 8 	(* 0x0100 *)
    | COB_FLAG_IS_FP -> 9       	(* 0x0200 *)
    | COB_FLAG_REAL_SIGN -> 10   	(* 0x0400 *)
    | COB_FLAG_BINARY_TRUNC -> 11	(* 0x0800 *)
    | COB_FLAG_CONSTANT -> 12    	(* 0x1000 *)

  let int_of_flags flags =
    let rec iter n flags =
      match flags with
      | [] -> n
      | flag :: flags ->
          iter (n lor (1 lsl (bit_of_flag flag))) flags
    in
    iter 0 flags

  (* init the libcob library *)
  external init : string array -> unit = "ml_cob_init"

  let init_needed = ref true
  let init args =
    if !init_needed then begin
      init args ;
      init_needed := false
    end

  (* clean exit *)
  external stop_run : status:int -> unit = "ml_cob_stop_run"

  (* create a COBOL module for this unit *)
  external module_create : name:string -> cob_module = "ml_cob_module_create"

  (* Enter the module (typically after a CALL) and register it. Return
     a context that contains the local information. *)
  external module_enter : cob_module -> context = "ml_cob_module_enter"

  (* Leave the module at the end of the execution of this CALL *)
  external module_leave : context -> unit = "ml_cob_module_leave"

  (* Free the module, it should never be used again *)
  external module_free : cob_module -> unit = "ml_cob_module_free"

  (* Allocates a cob_field_attr to describe a variable *)

  (* Specific values:
     ~digits: probably only used for COB_TYPE_NUMERIC_BINARY because it
       cannot be reliably computed ?
     ~scale: a part of the size that is not included in the representation
  *)

  external field_attr_create :
    ty:int ->
    digits:int ->
    scale:int ->
    flags:int ->
    pic:string option -> field_attr = "ml_cob_field_attr_create"
  let field_attr_create ?(digits=0) ?(scale=0) ?(flags=[]) ?pic ty =
    field_attr_create ~ty:(int_of_ty ty) ~digits ~scale
      ~flags:(int_of_flags flags) ~pic
  external field_attr_free : field_attr -> unit = "ml_cob_field_attr_free"
  external field_attr_get_ty :
    field_attr -> int = "ml_cob_field_attr_get_ty"
  let field_attr_get_ty attr = ty_of_int @@ field_attr_get_ty attr
  external field_attr_get_digits :
    field_attr -> int = "ml_cob_field_attr_get_digits"
  external field_attr_get_scale :
    field_attr -> int = "ml_cob_field_attr_get_scale"
  external field_attr_get_flags :
    field_attr -> int = "ml_cob_field_attr_get_flags"
  external field_attr_get_pic :
    field_attr -> string option = "ml_cob_field_attr_get_pic"

  (* Allocates a static buffer of the given size *)
  external buffer_create : int -> buffer = "ml_cob_buffer_create"
  external buffer_free : buffer -> unit = "ml_cob_buffer_free"
  let buffer_get_size { buf_size ; _ } = buf_size
  external buffer_get_addr : buffer -> int = "ml_cob_buffer_get_addr"
  external buffer_get_string : buffer -> string = "ml_cob_buffer_get_string"
  external buffer_get_substring :
    buffer -> int -> int -> string = "ml_cob_buffer_get_substring"

  (* Allocates a cob_field, using the static buffer at the given position *)
  external field_create :
    size:int ->
    buffer -> buf_pos:int ->
    field_attr -> field = "ml_cob_field_create"
  external field_free : field -> unit = "ml_cob_field_free"

  external field_get_size: field -> int = "ml_cob_field_get_size"
  external field_get_attr: field -> field_attr =
    "ml_cob_field_get_attr"
  external field_get_buffer: field -> buffer = "ml_cob_field_get_buffer"
  external field_set_buffer:
    field -> buffer -> buf_pos:int -> unit = "ml_cob_field_set_buffer"
  external subfield_set_pos:
    subfield:field -> field:field -> buf_pos:int -> unit = "ml_cob_subfield_set_pos"

  (* Copy the string into the field. The string must be the same len
     or smaller *)
  external field_init : field -> string -> unit = "ml_cob_field_init"

  (* Copy the field into the decimal register at the given position *)
  external decimal_set_field :
    context -> decimal -> src:field -> unit = "ml_cob_decimal_set_field"
  (* Copy the decimal register at the given position into the field *)
  external decimal_get_field :
    context -> decimal -> dst:field -> flags:int ->
    int = "ml_cob_decimal_get_field"
  (* Addition between two decimal registers *)
  external decimal_add :
    context -> decimal -> decimal -> unit = "ml_cob_decimal_add"

  (* Copy the given size from the src field to the dst field *)
  external memcpy :
    src:field -> dst:field -> int -> unit = "ml_cob_memcpy"

  (* Display the given fields. Currently limited to 8 arguments *)
  external display :
    to_device:int -> newline:bool -> field array -> unit
    = "ml_cob_display"
  let display ?(to_device=0) ?(newline=true) fields =
    display ~to_device ~newline fields

  external resolve_cobol_and_call: string -> int =
    "ml_cob_resolve_cobol_and_call"

  let field_attr_alphanum_constant =
    field_attr_create COB_TYPE_ALPHANUMERIC
      ~flags:[ COB_FLAG_CONSTANT ]

  let field_of_string s =
    let len = String.length s in
    let buf = buffer_create len in
    let c = field_create ~size:len buf ~buf_pos:0
        field_attr_alphanum_constant in
    field_init c s;
    c

(* The following line should be specified with an offset of +2 *)
#313 "_build/default/src/ezlibcob/v1.ml"
