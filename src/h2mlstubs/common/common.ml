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

type error =
  | IntegerOverflow

module type CHECKED_ARITH = sig
  type repr
  val min_int : repr
  val max_int : repr
  val zero : repr
  val one  : repr
  val of_int_unsafe : int -> repr
  val to_int_unsafe : repr -> int
  val add_unsafe : repr -> repr -> repr
  val sub_unsafe : repr -> repr -> repr
  val of_int  : int -> (repr, error) result
  val to_int  : repr -> (int, error) result
  val add : repr -> repr -> (repr, error) result
  val sub : repr -> repr -> (repr, error) result
  val compare : repr -> repr -> int
end

module BoundedInteger (B : sig val min : int val max : int end)
       : CHECKED_ARITH with type repr = int =
struct
  type repr = int
  let min_int = B.min
  let max_int = B.max
  let zero = 0
  let one = 1
  external of_int_unsafe : int -> repr = "%identity"
  external to_int_unsafe : repr -> int = "%identity"
  let add_unsafe a b = a + b
  let sub_unsafe a b = a - b
  let check i =
    if (i >= B.min && i <= B.max) then Ok (i)
    else Error (IntegerOverflow)
  let of_int i = check i
  let to_int r = Ok (r)
  let add a b = check (a + b)
  let sub a b = check (a - b)
  let compare = Stdlib.compare
end

module BoundedSignedInt64 : CHECKED_ARITH with type repr = int64 =
struct
  type repr = int64
  let min_int = Int64.min_int
  let max_int = Int64.max_int
  let zero = 0L
  let one = 1L
  let of_int_unsafe = Int64.of_int
  let to_int_unsafe = Int64.to_int
  let add_unsafe = Int64.add
  let sub_unsafe = Int64.sub
  let of_int i = Ok (Int64.of_int i)
  let to_int r =
    let i = Int64.to_int r in
    if Int64.equal (Int64.of_int i) r then Ok (i)
    else Error (IntegerOverflow)
  let add a b =
    let s = Int64.add a b in
    if (a >= 0L) = (b >= 0L) && (s >= 0L) <> (a >= 0L) then
      Error (IntegerOverflow)
    else Ok (s)
  let sub a b =
    let s = Int64.sub a b in
    if (a >= 0L) <> (b >= 0L) && (s >= 0L) <> (a >= 0L) then
      Error (IntegerOverflow)
    else Ok (s)
  let compare = Int64.compare
end

module BoundedUnsignedInt64 : CHECKED_ARITH with type repr = int64 =
struct
  type repr = int64
  let min_int = 0L
  let max_int = 0xFFFFFFFFFFFFFFFFL
  let zero = 0L
  let one = 1L
  let of_int_unsafe = Int64.of_int
  let to_int_unsafe = Int64.to_int
  let add_unsafe = Int64.add
  let sub_unsafe = Int64.sub
  let of_int i =
    if i < 0 then Error (IntegerOverflow)
    else Ok (Int64.of_int i)
  let to_int r =
    if Int64.unsigned_compare r (Int64.of_int Stdlib.max_int) > 0 then
      Error (IntegerOverflow)
    else Ok (Int64.to_int r)
  let add a b =
    let s = Int64.add a b in
    if Int64.unsigned_compare s a < 0 then
      Error (IntegerOverflow)
    else Ok s
  let sub a b =
    if Int64.unsigned_compare b a > 0 then
      Error (IntegerOverflow)
    else Ok (Int64.sub a b)
  let compare = Int64.unsigned_compare
end

module MakeCheckedInteger (A : CHECKED_ARITH) (T : sig type t end) =
struct
  external of_repr : A.repr -> T.t = "%identity"
  external to_repr : T.t -> A.repr = "%identity"

  let min_int = of_repr A.min_int
  let max_int = of_repr A.max_int
  let zero = of_repr A.zero
  let one  = of_repr A.one

  let of_int_unsafe i = of_repr (A.of_int_unsafe i)
  let to_int_unsafe t = A.to_int_unsafe (to_repr t)
  let succ_unsafe t   = of_repr (A.add_unsafe (to_repr t) A.one)
  let pred_unsafe t   = of_repr (A.sub_unsafe (to_repr t) A.one)
  let add_unsafe a b  = of_repr (A.add_unsafe (to_repr a) (to_repr b))
  let sub_unsafe a b  = of_repr (A.sub_unsafe (to_repr a) (to_repr b))
  let addi_unsafe a i = add_unsafe a (of_int_unsafe i)
  let subi_unsafe a i = sub_unsafe a (of_int_unsafe i)

  let map f r = Result.map of_repr (f (to_repr r))
  let map2 f a b = Result.map of_repr (f (to_repr a) (to_repr b))

  let of_int i = Result.map of_repr (A.of_int i)
  let to_int t = A.to_int (to_repr t)
  let succ t   = map (fun r -> A.add r A.one) t
  let pred t   = map (fun r -> A.sub r A.one) t
  let add a b  = map2 A.add a b
  let sub a b  = map2 A.sub a b
  let addi a i = Result.bind (of_int i) (add a)
  let subi a i = Result.bind (of_int i) (sub a)

  let compare a b = A.compare (to_repr a) (to_repr b)
end

module S8 = struct
  type k
  type t = k cint
  include MakeCheckedInteger
      (BoundedInteger (struct let min = -128 let max = 127 end))
      (struct type nonrec t = t end)
  external of_char : char -> t = "%identity"
  external to_char : t -> char = "%identity"
end

module U8 = struct
  type k
  type t = k cint
  include MakeCheckedInteger
      (BoundedInteger (struct let min = 0 let max = 256 end))
      (struct type nonrec t = t end)
  external of_char : char -> t = "%identity"
  external to_char : t -> char = "%identity"
end

module S16 = struct
  type k
  type t = k cint
  include MakeCheckedInteger
      (BoundedInteger (struct let min = -32768 let max = 32767 end))
      (struct type nonrec t = t end)
end

module U16 = struct
  type k
  type t = k cint
  include MakeCheckedInteger
      (BoundedInteger (struct let min = 0 let max = 65535 end))
      (struct type nonrec t = t end)
end

module S32 = struct
  type k
  type t = k cint
  include MakeCheckedInteger
      (BoundedInteger (struct let min = -2147483648 let max = 2147483647 end))
      (struct type nonrec t = t end)
end

module U32 = struct
  type k
  type t = k cint
  include MakeCheckedInteger
      (BoundedInteger (struct let min = 0 let max = 4294967295 end))
      (struct type nonrec t = t end)
end

module S64 = struct
  type k
  type t = k cint
  include MakeCheckedInteger (BoundedSignedInt64)
      (struct type nonrec t = t end)
  external of_int64 : int64 -> t = "%identity"
  external to_int64 : t -> int64 = "%identity"
end

module U64 = struct
  type k
  type t = k cint
  include MakeCheckedInteger (BoundedUnsignedInt64)
      (struct type nonrec t = t end)
  external of_int64 : int64 -> t = "%identity"
  external to_int64 : t -> int64 = "%identity"
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
