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

open EzCompat

module FATAL = Cobol_common.Diagnostics.Fatal

type kind =
  | DialectAll
  | DialectAllDevices
  | DialectAllSwitches
  | DialectAllFeatures
  | Word of string

let kind_of_string = function
  | "DIALECT-ALL" -> DialectAll
  | "DIALECT-ALL-DEVICES" -> DialectAllDevices
  | "DIALECT-ALL-SWITCHES" -> DialectAllSwitches
  | "DIALECT-ALL-FEATURES" -> DialectAllFeatures
  | s -> Word s

(** Module containing all the modules used to build all the reserved words sets *)

(*NOTE: no `mli` as this module is not to be used outside of `Cobol_config` *)

(* TODO: remove all the refs/mutability; for now we hide this using an
   applicative functor. *)

module type S = sig
  module RESERVED: sig
    val add_reserved : string -> unit
    val add_alias : string -> string -> unit
    val remove_reserved : string -> unit
    val words : unit -> Cobol_common.Reserved.TYPES.words_spec
  end
  module INTRINSIC: sig
    val add_intrinsic : string -> unit
    val remove_intrinsic : string -> unit
    val intrinsic_functions : unit -> StringSet.t
  end
  module SYSTEM_NAMES: sig
    val add_system_name : string -> unit
    val remove_system_name : string -> unit
    val system_names : unit -> StringSet.t
  end
  module REGISTERS: sig
    val add_register : string -> unit
    val remove_register : string -> unit
    val registers : unit -> StringSet.t
  end
end

module Make () : S = struct

(** This module is used to build reserved words set and aliases map. *)
module RESERVED = struct

  let conf : Cobol_common.Reserved.TYPES.words_spec ref = ref []
  let cons s = conf := s :: !conf

  let categorize word =
    if Str.last_chars word 1 = "*"
    then String.sub word 0 (String.length word - 1), true
    else word, false

  let add_reserved str =
    match kind_of_string str with
    | DialectAll ->
        conf := List.rev_append Cobol_common.Reserved.words !conf
    | Word str ->
      let w, preserve_context_sensitivity = categorize str in
      cons (w, ReserveWord { preserve_context_sensitivity })
    | _ -> Pretty.failwith "Wrong DIALECT-* word" str

  let add_alias alias alias_for =
    let w, preserve_context_sensitivity = categorize alias in
    cons (w, ReserveAlias { preserve_context_sensitivity; alias_for })

  let remove_reserved w =
    cons (w, NotReserved)

  let words () = List.rev !conf

end

(** Module used to build the intrinsic functions set *)
module INTRINSIC = struct
  let intrinsic = ref Cobol_common.Reserved.default_intrinsics

  let add_intrinsic word =
    match kind_of_string word with
    | DialectAll ->
        intrinsic := StringSet.union Cobol_common.Reserved.intrinsic_functions !intrinsic
    | Word word ->
        if not @@ StringSet.mem word Cobol_common.Reserved.intrinsic_functions then
          FATAL.error "Unknown intrinsic function %s" word
        else
          intrinsic := StringSet.add word !intrinsic
    | _ -> Pretty.failwith "Wrong DIALECT-* word" word

  let remove_intrinsic word =
    match kind_of_string word with
    | DialectAll ->
        intrinsic :=
          StringSet.filter
            (fun v -> not @@ StringSet.mem v
                Cobol_common.Reserved.intrinsic_functions) !intrinsic
    | Word word ->
        intrinsic := StringSet.remove word !intrinsic
    | _ -> Pretty.failwith "Wrong DIALECT-* word" word

  let intrinsic_functions () = !intrinsic
end

(** Module used to build the system names set *)
module SYSTEM_NAMES = struct
  let system_names = ref Cobol_common.Reserved.default_system_names

  let add_system_name name =
    match kind_of_string name with
    | DialectAll ->
        system_names := StringSet.union Cobol_common.Reserved.system_names !system_names
    | DialectAllDevices ->
        system_names := StringSet.union Cobol_common.Reserved.device_system_names !system_names
    | DialectAllSwitches ->
        system_names := StringSet.union Cobol_common.Reserved.switch_system_names !system_names
    | DialectAllFeatures ->
        system_names := StringSet.union Cobol_common.Reserved.feature_system_names !system_names
    | Word name ->
        if StringSet.mem name Cobol_common.Reserved.system_names then
          system_names := StringSet.add name !system_names
        else
          FATAL.error "Unknown system name: %s" name

  let remove_system_name name =
    match kind_of_string name with
    | DialectAll ->
      system_names := StringSet.diff !system_names Cobol_common.Reserved.system_names
    | DialectAllDevices ->
      system_names := StringSet.diff !system_names Cobol_common.Reserved.device_system_names
    | DialectAllSwitches ->
      system_names := StringSet.diff !system_names Cobol_common.Reserved.switch_system_names
    | DialectAllFeatures ->
      system_names := StringSet.diff !system_names Cobol_common.Reserved.feature_system_names
    | Word name ->
      system_names := StringSet.remove name !system_names

  let system_names () = !system_names
end

(** Module used to build the registers set *)
module REGISTERS = struct
  let registers = ref Cobol_common.Reserved.default_registers

  let add_register register =
    match kind_of_string register with
    | DialectAll ->
        registers := StringSet.union !registers Cobol_common.Reserved.default_registers
    |  Word register ->
        if not @@ StringSet.mem register Cobol_common.Reserved.registers then
          FATAL.error "Unknown register: %s" register
        else
          registers := StringSet.add register !registers
    | _ -> Pretty.failwith "Wrong DIALECT-* word %S" register

  let remove_register register =
    match kind_of_string register with
    | DialectAll ->
        registers := StringSet.diff !registers Cobol_common.Reserved.default_registers
    | Word register ->
        registers := StringSet.remove register !registers
    | _ -> Pretty.failwith "Wrong DIALECT-* word %S" register

  let registers () = !registers
end

end
