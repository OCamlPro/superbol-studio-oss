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

(** Environment for preprocessing and compilation.  Holds definitions of
    preprocessor constants (preprocessor DEFINES, computation process
    variables), along with compilation variables (78-level constants).

    Accepted values for these preprocessor variables are:
    - Alphanumerics;
    - Booleans;
    - Fixed-point numerics (no plain Integer, nor floating-points).

    For now, the set of values for compilation variables is equivalent to that
    of preprocessor variables. *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_data.Types

module VAL = Cobol_data.Value

(** Utility module that maps any string to a physically unique upper-cased
    internal representation. *)
module VAR: sig
  type t
  val pp: t Pretty.printer
  val of_string: string -> t
  val to_uppercase_string: t -> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
end = struct
  module TBL = Ephemeron.K1.Make (struct include String let hash = Hashtbl.hash end)
  type t = string
  let tbl = TBL.create 17                                                 (* arbitrary *)
  let pp = Fmt.string
  let of_string s =
    let s' = String.uppercase_ascii s in
    try TBL.find tbl s'
    with Not_found -> TBL.add tbl s' s'; s'
  let to_uppercase_string = Fun.id
  let compare = String.compare
  let equal = (==)
end

module MAP = Map.Make (VAR)

module TYPES = struct
  type env =
    {
      preproc_vars: preproc_var_definition MAP.t;
      compil_vars: compilation_var_definition MAP.t;
    }

  and var = VAR.t

  and preproc_var_definition =
    compilation_variable_definition with_src

  and compilation_var_definition =
    compilation_variable_definition with_src       (* same for now (not sure) *)

  and compilation_variable_definition =
    {
      compvar: var;
      compvar_value: value with_src;
    }

  and value =
    | Alphanum of Cobol_data.Types.alphanum_value
    | Boolean of Cobol_data.Types.boolean_value
    | Numeric of Cobol_data.Types.fixed_value

  type var_definition =
    | Preproc_var of preproc_var_definition
    | Compilation_var of compilation_var_definition

  type lookup_error =
    | Undefined

  exception REDEFINITION of { prev_def_src: src }
end
include TYPES

type t = env

(* pretty-printing *)

let pp_value ppf = function
  | Alphanum s -> Cobol_data.Printer.pp_alphanum_value ppf s
  | Boolean b -> Cobol_data.Printer.pp_boolean_value ppf b
  | Numeric f -> Cobol_data.Printer.pp_fixed_value ppf f

let pp_compilation_variable_definition ppf { compvar; compvar_value } =
  Pretty.record [
    Fmt.field "name" (fun () -> compvar) VAR.pp;
    Fmt.field "value" (fun () -> compvar_value) (pp_with_src pp_value);
  ] ppf ()

let pp: t Pretty.printer = fun ppf map ->
  Pretty.list ~fopen:"@[<2>@<1>⦃ " ~fsep:",@ " ~fclose:" @<1>⦄@]"
    Fmt.(box ~indent:2 @@ pair ~sep:(any " =>@ ") VAR.pp
           (pp_with_src pp_compilation_variable_definition))
    ppf (MAP.bindings map.preproc_vars)

(* constructors *)

let empty =
  {
    preproc_vars = MAP.empty;
    compil_vars = MAP.empty;
  }

let var: string -> var = VAR.of_string
let var': string with_loc -> var with_loc = Cobol_common.Srcloc.map_payload var

let mem_preproc_var v env = MAP.mem v env.preproc_vars
let mem_preproc_var' v env = MAP.mem ~&v env.preproc_vars

let mem_compil_var v env = MAP.mem v env.compil_vars
let mem_compil_var' v env = MAP.mem ~&v env.compil_vars

let mem_var v env = mem_preproc_var v env || mem_compil_var v env
let mem_var' v env = mem_preproc_var ~&v env || mem_compil_var ~&v env

(* higher-level operations *)

let var_definition_of ~var ?(try_compil_vars = true) env
  : (var_definition, lookup_error) result =
  match MAP.find_opt ~&var env.preproc_vars with
  | Some value ->
      Ok (Preproc_var value)
  | None ->
      if try_compil_vars then
        match MAP.find_opt ~&var env.compil_vars with
        | Some value -> Ok (Compilation_var value)
        | None -> Error Undefined
      else
        Error Undefined

let register_preproc_var ~src var value env =
  let def =
    Cobol_common.Srcloc.with_src ~src
      { compvar = var; compvar_value = value }
  in
  { env with
    preproc_vars = MAP.add var def env.preproc_vars },
  def

let define_preproc_var ~loc var value ?(override = false) (env: t)
  : t * preproc_var_definition =
  match MAP.find_opt ~&var env.preproc_vars with
  | Some { src; _ } when not override ->
      raise @@ REDEFINITION { prev_def_src = src }
  | Some _ | None ->
      register_preproc_var ~&var value env ~src:(Source_location loc)

let define_process_parameter var value (env: t) =          (* always override *)
  register_preproc_var var value env ~src:Process_parameter

let undefine_preproc_var var (env: t) : t =
  { env with preproc_vars = MAP.remove ~&var env.preproc_vars }

(* --- *)

let define_compilation_var ~loc var value (env: t)
  : t * compilation_var_definition =
  let def =
    Cobol_common.Srcloc.with_loc_as_src ~loc
      { compvar = ~&var; compvar_value = value }
  in
  { env with compil_vars = MAP.add ~&var def env.compil_vars },
  def

let find_compilation_var v env =
  MAP.find_opt v env.compil_vars

(* --- *)

let alphanum_literal_value' (a: alphanum_literal with_loc) : value =
  Alphanum ~&a

let boolean_literal_value' (b: boolean_literal with_loc) : value =
  Boolean ~&b.bool_value

let numeric_literal_value' (f: fixed_literal with_loc) : value =
  Numeric ~&f.fixed_value

let alphanum_literal_value (a: alphanum_literal with_loc) : value with_src =
  Cobol_common.Srcloc.with_loc_as_src ~loc:~@a (alphanum_literal_value' a)

let boolean_literal_value (b: boolean_literal with_loc) : value with_src =
  Cobol_common.Srcloc.with_loc_as_src ~loc:~@b (boolean_literal_value' b)

let numeric_literal_value (f: fixed_literal with_loc) : value with_src =
  Cobol_common.Srcloc.with_loc_as_src ~loc:~@f (numeric_literal_value' f)
