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

(** This module aims to implement mangling functions for the COBOL AST.*)


open Cobol_ast

(** This exception is raised when a not mangled name is given in a context where it is expected for
    the name to be mangled *)
exception Not_mangled

(** This function mangles an entry name option, by changing every [None] or [Some (Filler)] entries
    into [Some (Name "Filler<n>")] where <n> is incremented every time a new [None] or [Some (Filler)]
    entry is encountered. It does not change any other [entry_name] value. *)
val mangle_data_name
  : default_loc: Cobol_common.Srcloc.srcloc
  -> data_name with_loc option
  -> data_name with_loc option

(** This function returns the name as a string if it is in the form of [Some (Name name)] and
    raises the [Not_mangled] exception if the pattern of the [entry_name] is different. *)
val mangled_data_name: data_name with_loc option -> string
