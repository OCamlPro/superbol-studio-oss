(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Representation of COBOL literals *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Data_types

module VAL = Data_value

(* --- *)

let invalid_chars ~loc ~literal_class chars =
  NEL.map ~f:begin fun (i, c) ->
    let loc = Cobol_common.Srcloc.trunc_prefix i loc in
    let loc = Cobol_common.Srcloc.prefix 1 loc in
    Invalid { loc; stuff = Character_in_literal { literal_class; char = c } }
  end chars

(* --- *)

let alphanum_of_string ?quotation (str: string) : alphanum_literal =
  { alphanum_ptree = Cobol_ptree.alphanum_of_string ?quotation str;
    alphanum_value = VAL.plain_alphanum str }

let alphanum_with_dummy_fallback (lit: Cobol_ptree.alphanum with_loc)
  : alphanum_literal with_loc * errors option =
  match VAL.alphanum ~&lit with
  | Ok alphanum_value ->
      { alphanum_ptree = ~&lit; alphanum_value } &@<- lit, None
  | Error Invalid_chars chars ->
      (* only erroring case is for hexadecimal literals: *)
      { alphanum_ptree = ~&lit; alphanum_value = ~&lit.str } &@<- lit,
      Some (invalid_chars ~loc:~@lit ~literal_class:Hexadecimal chars)

(* --- *)

let integer_with_dummy_fallback (lit: Cobol_ptree.integer with_loc)
  : integer_literal with_loc * errors option =
  match VAL.integer ~&lit with
  | Ok int_value ->
      { int_ptree = ~&lit; int_value } &@<- lit, None
  | Error Invalid_chars chars ->
      { int_ptree = ~&lit; int_value = VAL.integer_zero } &@<- lit,
      Some (invalid_chars ~loc:~@lit ~literal_class:Integer chars)

(* --- *)

let fixed_with_dummy_fallback (lit: Cobol_ptree.fixed with_loc)
  : fixed_literal with_loc * errors option =
  match VAL.fixed ~&lit with
  | Ok fixed_value ->
      { fixed_ptree = ~&lit; fixed_value } &@<- lit, None
  | Error Invalid_chars chars ->
      { fixed_ptree = ~&lit; fixed_value = VAL.fixed_zero } &@<- lit,
      Some (invalid_chars ~loc:~@lit ~literal_class:Fixed chars)

(* --- *)

let floating_with_dummy_fallback (lit: Cobol_ptree.floating with_loc)
  : floating_literal with_loc * errors option =
  match VAL.floating ~&lit with
  | Ok float_value ->
      { float_ptree = ~&lit; float_value } &@<- lit, None
  | Error Invalid_chars chars ->
      { float_ptree = ~&lit; float_value = VAL.floating_zero } &@<- lit,
      Some (invalid_chars ~loc:~@lit ~literal_class:Floating chars)

(* --- *)

let boolean_with_dummy_fallback
    (* TODO deal with prefix length? *)
    ?(max_length = 8_191)                         (* as per ISO/IEC 1989:2014 *)
    (lit: Cobol_ptree.boolean with_loc) =
  let literal_string = ~&lit.bool_string and base = ~&lit.bool_base in
  let length_error =
    if String.length literal_string > max_length
    then Some (Overlong_literal { loc = ~@lit; literal_string; max_length })
    else None
  in
  let errors errors =
    match length_error, errors with
    | None, None -> None
    | Some e, None -> Some (NEL.one e)
    | None, Some el -> Some el
    | Some e, Some el -> Some NEL.(e :: el)
  in
  match VAL.boolean ~&lit with
  | Ok bool_value ->
      { bool_ptree = ~&lit; bool_value } &@<- lit, errors None
  | Error Invalid_chars chars ->
      { bool_ptree = ~&lit; bool_value = VAL.boolean_zero } &@<- lit,
      let literal_class = if base = `Bool then Boolean else Hexadecimal in
      errors @@ Some (invalid_chars ~loc:~@lit ~literal_class chars)

(* --- *)

let value_or_error ~literal_class ~loc = function
  | Ok x ->
      Ok (x &@ loc)
  | Error VAL.Invalid_chars chars ->
      Error (invalid_chars ~loc ~literal_class chars)

let alphanum_value x : (alphanum_value with_loc, errors) result =
  value_or_error ~literal_class:Hexadecimal ~loc:~@x (VAL.alphanum ~&x)

let boolean_value x : (boolean_value with_loc, errors) result =
  value_or_error ~literal_class:Boolean ~loc:~@x (VAL.boolean ~&x)

let integer_value x : (integer_value with_loc, errors) result =
  value_or_error ~literal_class:Hexadecimal ~loc:~@x (VAL.integer ~&x)

let fixed_value x : (fixed_value with_loc, errors) result =
  value_or_error ~literal_class:Hexadecimal ~loc:~@x (VAL.fixed ~&x)

let floating_value x : (floating_value with_loc, errors) result =
  value_or_error ~literal_class:Hexadecimal ~loc:~@x (VAL.floating ~&x)

let alphanum_literal_value x : (literal_value with_loc, errors) result =
  Result.map (fun x -> Alphanum_value ~&x &@<- x) (alphanum_value x)

let boolean_literal_value x : (literal_value with_loc, errors) result =
  Result.map (fun x -> Boolean_value ~&x &@<- x) (boolean_value x)

let integer_literal_value x : (literal_value with_loc, errors) result =
  Result.map (fun x -> Integer_value ~&x &@<- x) (integer_value x)

let fixed_literal_value x : (literal_value with_loc, errors) result =
  Result.map (fun x -> Fixed_value ~&x &@<- x) (fixed_value x)

let floating_literal_value x : (literal_value with_loc, errors) result =
  Result.map (fun x -> Floating_value ~&x &@<- x) (floating_value x)

(* --- *)

let rec value
    (lit: Cobol_ptree.literal with_loc)
  : (literal_value with_loc, errors) result =
  match ~&lit with
  | Alphanum x ->
      alphanum_literal_value (x &@<- lit)
  | Boolean x ->
      boolean_literal_value (x &@<- lit)
  | Integer x ->
      integer_literal_value (x &@<- lit)
  | Fixed x ->
      fixed_literal_value (x &@<- lit)
  | Floating x ->
      floating_literal_value (x &@<- lit)
  | NumFig Zero
  | Fig Zero ->
      Ok (Zero_value &@<- lit)
  | Fig Space ->
      Ok (Space_value &@<- lit)
  | Fig Quote ->
      Ok (Quote_value &@<- lit)
  | Fig LowValue ->
      Ok (Low_value &@<- lit)
  | Fig HighValue ->
      Ok (High_value &@<- lit)
  | StrConcat (a, b) ->
      concat ~loc:~@lit (strlit_value a) (strlit_value b)
  | Concat (a, b) ->
      concat ~loc:~@lit (nonnumlit_value a) (nonnumlit_value b)
  | Fig (All _ as x) ->
      unsupported ~loc:~@lit (Figurative_constant x)
  | National _ ->
      unsupported ~loc:~@lit National_literal

and strlit_value: Cobol_ptree.strlit with_loc -> _ = fun lit ->
  match ~&lit with
  | Alphanum x ->
      alphanum_literal_value (x &@<- lit)
  | Fig Zero ->
      Ok (Zero_value &@<- lit)
  | Fig Space ->
      Ok (Space_value &@<- lit)
  | Fig Quote ->
      Ok (Quote_value &@<- lit)
  | Fig LowValue ->
      Ok (Low_value &@<- lit)
  | Fig HighValue ->
      Ok (High_value &@<- lit)
  | StrConcat (a, b) ->
      concat ~loc:~@lit (strlit_value a) (strlit_value b)
  | Fig (All _ as x) ->
      unsupported ~loc:~@lit (Figurative_constant x)
  | National _ ->
      unsupported ~loc:~@lit National_literal

and nonnumlit_value: Cobol_ptree.nonnumlit with_loc -> _ = fun lit ->
  match ~&lit with
  | Alphanum x ->
      alphanum_literal_value (x &@<- lit)
  | Boolean x ->
      boolean_literal_value (x &@<- lit)
  | Fig Zero ->
      Ok (Zero_value &@<- lit)
  | Fig Space ->
      Ok (Space_value &@<- lit)
  | Fig Quote ->
      Ok (Quote_value &@<- lit)
  | Fig LowValue ->
      Ok (Low_value &@<- lit)
  | Fig HighValue ->
      Ok (High_value &@<- lit)
  | StrConcat (a, b) ->
      concat ~loc:~@lit (strlit_value a) (strlit_value b)
  | Concat (a, b) ->
      concat ~loc:~@lit (nonnumlit_value a) (nonnumlit_value b)
  | Fig (All _ as x) ->
      unsupported ~loc:~@lit (Figurative_constant x)
  | National _ ->
      unsupported ~loc:~@lit National_literal

and concat ~loc a b =
  match a, b with
  | Error e, Error f ->
      Error (NEL.append e f)
  | Error e, Ok _ | Ok _, Error e ->
      Error e
  | Ok { payload = Alphanum_value a; _ },
    Ok { payload = Alphanum_value b; _ } ->
      Ok (Alphanum_value (VAL.concat_alphanums a b) &@ loc)
  | Ok _, Ok _ ->                                        (* Not suported (yet) *)
      unsupported ~loc Concatenation_of_literals

and unsupported ~loc stuff =
  Error (NEL.one @@ Unsupported { loc; stuff })
