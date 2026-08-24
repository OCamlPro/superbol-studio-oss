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
module OUT = Data_diagnostics

(* --- *)

let error diags e = OUT.add_error e diags

let invalid_chars ~loc ~literal_class diags chars =
  VAL.NEL.fold_left ~f:begin fun diags (i, c) ->
    let loc = Cobol_common.Srcloc.trunc_prefix i loc in
    let loc = Cobol_common.Srcloc.prefix 1 loc in
    error diags @@
    Invalid { loc; stuff = Character_in_literal { literal_class; char = c } }
  end diags chars

let with_invalid_chars ~loc ~literal_class diags chars v =
  let diags = invalid_chars ~loc diags chars ~literal_class in
  OUT.result ~diags (v &@ loc)

(* --- *)

let alphanum ({ payload = literal; loc }: Cobol_ptree.alphanum with_loc)
  : alphanum_literal with_loc OUT.with_diags =
  try
    OUT.result ({ alphanum_ptree = literal;
                  alphanum_value = VAL.alphanum literal} &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Hexadecimal
      { alphanum_ptree = literal; alphanum_value = literal.str }

(* --- *)

let integer ({ payload = literal; loc }: Cobol_ptree.integer with_loc)
  : integer_literal with_loc OUT.with_diags =
  try
    OUT.result ({ int_ptree = literal;
                  int_value = VAL.integer literal } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Integer
      { int_ptree = literal; int_value = VAL.integer_zero }

(* --- *)

let fixed ({ payload = literal; loc }: Cobol_ptree.fixed with_loc)
  : fixed_literal with_loc OUT.with_diags =
  try
    OUT.result ({ fixed_ptree = literal;
                  fixed_value = VAL.fixed literal } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Fixed
      { fixed_ptree = literal; fixed_value = VAL.fixed_zero }

(* --- *)

let floating ({ payload = literal; loc }: Cobol_ptree.floating with_loc)
  : floating_literal with_loc OUT.with_diags =
  try
    OUT.result ({ float_ptree = literal;
                  float_value = VAL.floating literal } &@ loc)
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc OUT.none chars ~literal_class:Floating
      { float_ptree = literal; float_value = VAL.floating_zero }


(* --- *)

let boolean
    (* TODO deal with prefix length? *)
    ?(max_length = 8_191)                         (* as per ISO/IEC 1989:2014 *)
    Cobol_ptree.{ payload = { bool_base = base;
                              bool_string = literal_string } as bool_ptree;
                  loc } =
  let diags = OUT.none in
  let len = String.length literal_string in
  let diags =
    if len > max_length
    then error diags @@ Overlong_literal { loc; literal_string; max_length }
    else diags
  in
  try
    OUT.result ({ bool_ptree;
                  bool_value = VAL.boolean bool_ptree } &@ loc)
      ~diags
  with VAL.INVALID_CHARS chars ->
    with_invalid_chars ~loc diags chars
      ~literal_class:(if base = `Bool then Boolean else Hexadecimal)
      { bool_ptree; bool_value = VAL.boolean_zero }

(* --- *)

let rec value: Cobol_ptree.literal with_loc -> (literal_value with_loc, _) result = fun lit ->
  try
    Result.map (fun x -> x &@<- lit) @@
    match ~&lit with
    | Alphanum a ->
        Ok (Alphanum_value (VAL.alphanum a))
    | Boolean b ->
        Ok (Boolean_value (VAL.boolean b))
    | Integer i ->
        Ok (Integer_value (VAL.integer i))
    | Fixed f ->
        Ok (Fixed_value (VAL.fixed f))
    | Floating f ->
        Ok (Floating_value (VAL.floating f))
    | NumFig Zero
    | Fig Zero ->
        Ok Zero_value
    | Fig Space ->
        Ok Space_value
    | Fig Quote ->
        Ok Quote_value
    | Fig LowValue ->
        Ok Low_value
    | Fig HighValue ->
        Ok High_value
    | StrConcat (a, b) ->
        concat (strlit_value a) (strlit_value b)
    | Concat (a, b) ->
        concat (nonnumlit_value a) (nonnumlit_value b)
    | Fig All _
    | National _ ->
        Error ()
  with VAL.INVALID_CHARS _ | Exit ->
    Error ()

and strlit_value: Cobol_ptree.strlit with_loc -> _ = fun lit ->
  try
    Result.map (fun x -> x &@<- lit) @@
    match ~&lit with
    | Alphanum a ->
        Ok (Alphanum_value (VAL.alphanum a))
    | Fig Zero ->
        Ok Zero_value
    | Fig Space ->
        Ok Space_value
    | Fig Quote ->
        Ok Quote_value
    | Fig LowValue ->
        Ok Low_value
    | Fig HighValue ->
        Ok High_value
    | StrConcat (a, b) ->
        concat (strlit_value a) (strlit_value b)
    | Fig All _
    | National _ ->
        Error ()
  with VAL.INVALID_CHARS _ | Exit ->
    Error ()

and nonnumlit_value: Cobol_ptree.nonnumlit with_loc -> _ = fun lit ->
  try
    Result.map (fun x -> x &@<- lit) @@
    match ~&lit with
    | Alphanum a ->
        Ok (Alphanum_value (VAL.alphanum a))
    | Boolean b ->
        Ok (Boolean_value (VAL.boolean b))
    | Fig Zero ->
        Ok Zero_value
    | Fig Space ->
        Ok Space_value
    | Fig Quote ->
        Ok Quote_value
    | Fig LowValue ->
        Ok Low_value
    | Fig HighValue ->
        Ok High_value
    | StrConcat (a, b) ->
        concat (strlit_value a) (strlit_value b)
    | Concat (a, b) ->
        concat (nonnumlit_value a) (nonnumlit_value b)
    | Fig All _
    | National _ ->
        Error ()
  with VAL.INVALID_CHARS _ | Exit ->
    Error ()

and concat a b =
  match a, b with
  | Error (), _ | _, Error () ->
      Error ()
  | Ok { payload = Alphanum_value a; _ },
    Ok { payload = Alphanum_value b; _ } ->
      Ok (Alphanum_value (VAL.concat_alphanums a b))
  | Ok _, Ok _ ->                                        (* Not suported (yet) *)
      Error ()
