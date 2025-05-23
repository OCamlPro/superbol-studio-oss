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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

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
  type env = definition MAP.t
  and var = VAR.t
  and definition =
    {
      def_loc: definition_loc;
      def_value: value;
    }
  and definition_loc = preproc_loc
  and 'a with_preproc_loc =
    { pp_payload: 'a; pp_loc: preproc_loc }
  and preproc_loc =
    | Source_location of srcloc
    | Process_parameter
    | Process_environment
    (* | Computed *)
  and value =
    | Alphanum of Cobol_data.Value.alphanum with_preproc_loc
    | Boolean of Cobol_data.Value.boolean with_preproc_loc
    | Numeric of Cobol_data.Value.fixed with_preproc_loc

  exception UNDEFINED of var with_loc
  exception REDEFINITION of { prev_def_loc: definition_loc }
end
include TYPES

type t = env

(* pretty-printing *)

let pp_value ppf = function
  | Alphanum s -> Pretty.print ppf "%s" s.pp_payload
  | Boolean b -> Pretty.print ppf "%a" Cobol_data.Value.pp_boolean b.pp_payload
  | Numeric f -> Pretty.print ppf "%a" Cobol_data.Value.pp_fixed f.pp_payload

let pp_definition ppf { def_value; _ } =
  pp_value ppf def_value

let pp: t Pretty.printer = fun ppf map ->
  Pretty.list ~fopen:"@[<2>@<1>⦃ " ~fsep:",@ " ~fclose:" @<1>⦄@]"
    Fmt.(box ~indent:2 @@ pair ~sep:(any " =>@ ") VAR.pp pp_definition)
    ppf (MAP.bindings map)

(* constructors *)

let empty = MAP.empty

let var = VAR.of_string
let var' = Cobol_common.Srcloc.map_payload var

let mem v = MAP.mem v
let mem' v = MAP.mem ~&v

(* higher-level operations *)

let definition_of ~var env : definition =
  match MAP.find_opt ~&var env with
  | None -> raise @@ UNDEFINED var
  | Some value -> value

let define ~loc var value ?(override = false) (env: t) : t =
  match MAP.find_opt ~&var env with
  | Some { def_loc; _ } when not override ->
      raise @@ REDEFINITION { prev_def_loc = def_loc }
  | Some _ | None ->
      MAP.add ~&var { def_loc = Source_location loc;
                      def_value = value } env

let define_process_parameter var value (env: t) : t =      (* always override *)
  MAP.add var { def_loc = Process_parameter;
                def_value = value } env

let undefine var (env: t) : t =
  MAP.remove ~&var env
