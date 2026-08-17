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

(** Environment for preprocessing and compilation.  Holds defintions of
    preprocessor constants (preprocessor DEFINES, compulation process
    variables), along with compilation variables (78-level constants).


    Accepted values for these preprocessor variables are:
    - Alphanumerics;
    - Booleans;
    - Fixed-point numerics (no plain Integer, nor floating-points).

    For now, the set of values for compilation variables if equivalent to that
    of preprocssor variables. *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_data.Types

module VAL = Cobol_data.Value

(** Utility module that maps any string to a physically unique upper-cased
    internal representation. *)
(* TODO: Maybe move that ot cobol_data as well *)
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
      preproc_vars: preproc_definition MAP.t;
      compil_vars: compilation_var_definition MAP.t;
    }
  and var = VAR.t
  and preproc_definition =
    Cobol_data.Types.compilation_variable_definition with_src
  and compilation_var_definition =
    preproc_definition                                  (* for now (not sure) *)
  and value =
    Cobol_data.Types.compilation_value

  exception REDEFINITION of { prev_def_src: src }
end
include TYPES

type t = env

(* pretty-printing *)

let pp: t Pretty.printer = fun ppf map ->
  Pretty.list ~fopen:"@[<2>@<1>⦃ " ~fsep:",@ " ~fclose:" @<1>⦄@]"
    Fmt.(box ~indent:2 @@ pair ~sep:(any " =>@ ") VAR.pp
           (pp_with_src Cobol_data.Printer.pp_compilation_variable_definition))
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

let preproc_var_definition_of ~var ?(try_compil_vars = true) env
  : (preproc_definition, [`UNDEFINED]) result =
  match MAP.find_opt ~&var env.preproc_vars with
  | Some value ->
      Ok value
  | None ->
      if try_compil_vars then
        match MAP.find_opt ~&var env.compil_vars with
        | Some value -> Ok value
        | None -> Error `UNDEFINED
      else
        Error `UNDEFINED

let register_preproc_var ~src var value env =
  let def =
    { compvar_name = VAR.to_uppercase_string var;
      compvar_value = value }
  in
  { env with
    preproc_vars = MAP.add var { src; src_payload = def } env.preproc_vars }

let define_preproc_var ~loc var value ?(override = false) (env: t) : t =
  match MAP.find_opt ~&var env.preproc_vars with
  | Some { src; _ } when not override ->
      raise @@ REDEFINITION { prev_def_src = src }
  | Some _ | None ->
      register_preproc_var ~&var value env ~src:(Source_location loc)

let define_process_parameter var value (env: t) : t =      (* always override *)
  register_preproc_var var value env ~src:Process_parameter

let undefine_preproc_var var (env: t) : t =
  { env with preproc_vars = MAP.remove ~&var env.preproc_vars }

(* --- *)

let define_compilation_var ~loc var value (env: t)
  : t * compilation_var_definition =
  let def =
    { src = Source_location loc;
      src_payload = { compvar_name = VAR.to_uppercase_string ~&var;
                      compvar_value = value } }
  in
  { env with compil_vars = MAP.add ~&var def env.compil_vars },
  def

let find_compilation_var v env =
  MAP.find_opt v env.compil_vars

(* --- *)

let alphanum_literal_value (a: alphanum_literal with_loc) : value =
  Alphanum { src_payload = ~&a;
             src = Source_location ~@a }

let boolean_literal_value (b: boolean_literal with_loc) : value =
  Boolean { src_payload = ~&b.bool_value;
            src = Source_location ~@b }

let numeric_literal_value (f: fixed_literal with_loc) : value =
  Numeric { src_payload = ~&f.fixed_value;
            src = Source_location ~@f }
