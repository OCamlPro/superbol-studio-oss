#2 "src/h2mlstubs/common/common.ml"
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

type !'a comp_kind

type !'a cint
type !'a enum
type !'a comp
type !'a cptr
type !'a carray

module S8 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  external of_char : char -> t = "%identity"
  external to_char : t -> char = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module U8 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  external of_char : char -> t = "%identity"
  external to_char : t -> char = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module S16 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module U16 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module S32 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module U32 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module S64 = struct
  type k
  type t = k cint
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module U64 = struct
  type k
  type t = k cint
  external of_int64 : int64 -> t = "%identity"
  external to_int64 : t -> int64 = "%identity"
  let of_int i = of_int64 (Int64.of_int i)
  let to_int i = Int64.to_int (to_int64 i)
  let zero = of_int 0
  let one = of_int 1
  let succ i = of_int (to_int i + 1)
  let pred i = of_int (to_int i - 1)
  let add i1 i2 = of_int (to_int i1 + to_int i2)
  let sub i1 i2 = of_int (to_int i1 - to_int i2)
  let addi i1 i2 = of_int (to_int i1 + i2)
  let subi i1 i2 = of_int (to_int i1 + i2)
end

module CFloat = struct
  type t
  external of_float : int -> t = "%identity"
  external to_float : t -> int = "%identity"
end

module CDouble = struct
  type t
  external of_float : float -> t = "%identity"
  external to_float : t -> float = "%identity"
end

type _ kind =
  | Void : unit kind
  | Char : char kind
  | SInt8 : S8.t kind
  | UInt8 : U8.t kind
  | SInt16 : S16.t kind
  | UInt16 : U16.t kind
  | SInt32 : S32.t kind
  | UInt32 : U32.t kind
  | SInt64 : S64.t kind
  | UInt64 : U64.t kind
  | CFloat : CFloat.t kind
  | CDouble : CDouble.t kind
  | CEnum : 'a enum kind
  | CComp : 'a comp_kind -> 'a comp kind
  | CPtr : 'a kind -> 'a cptr kind
  | CArray : 'a kind * int -> 'a carray kind

module CPtr = struct
  type 'a t = 'a cptr
  external null : 'a kind -> 'a t = "ml_ptr_null"
  external create : ?default:'a -> 'a kind -> 'a t = "ml_ptr_create"
  external free : 'a t -> unit = "ml_ptr_free"
  external get : 'a t -> 'a = "ml_ptr_get"
  external set : 'a t -> 'a -> unit = "ml_ptr_set"
  external cast : 'b kind -> 'a t -> 'b t = "ml_ptr_cast"
  external add : 'a t -> int -> 'a t = "ml_ptr_add"
  external sub : 'a t -> int -> 'a t = "ml_ptr_sub"
  external diff : 'a t -> 'a t -> int = "ml_ptr_diff"
end

module CArray = struct
  type 'a t = 'a carray
  external create : ?default:'a -> 'a kind -> int -> 'a t = "ml_array_create"
  external free : 'a t -> unit = "ml_array_free"
  external get : 'a t -> int -> 'a = "ml_array_get"
  external get_ptr : 'a t -> int -> 'a cptr = "ml_array_get_ptr"
  external set : 'a t -> int -> 'a -> unit = "ml_array_set"
  external to_ptr : 'a t -> 'a cptr = "ml_array_to_ptr"
  external of_ptr : int -> 'a cptr -> 'a t = "ml_array_of_ptr"
  external blit : 'a t -> int -> 'a t -> int -> int -> unit = "ml_array_blit"
  external of_string : string -> char t = "ml_array_of_string"
  external to_string : char t -> string = "ml_array_to_string"
  external get_string : char t -> int -> int -> string = "ml_array_get_string"
  external set_string : char t -> int -> string -> unit = "ml_array_set_string"
end
