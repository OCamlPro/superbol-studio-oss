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

(** This module implements a hierarchical version of cobol data items.*)
open Cobol_ptree
(* open Pictured_ast.Data_sections *)
open Cobol_common.Srcloc.TYPES

(** The type of hierarchical data items from cobol, used to describe different sections of the
    data division such as working storage section or linkage section. *)
type t' =                                              (* TODO: extract `name` *)
  | Renames of { name: name; targets: t list }
  | ConditionName of { name: name; values: condition_name_value list; target: t }
  | Constant of { name: name; value: constant_value with_loc;
                  constant_item_descr: constant_item_descr }
  | Elementary of { name: name; data_item: data_item_descr }
  | Group of { name: name; elements: t list; data_item: data_item_descr }
[@@deriving show, ord]

and t = t' with_loc
[@@deriving show, ord]

val pp_data_group_list: Format.formatter -> t list -> unit

(* (\** Extract the name from any kind of data item. *\) *)
(* val name_of: t -> name *)

(* (\** Extract the location of the name of a data group. *\) *)
(* val name_location: t -> srcloc *)

(** Convert a list of located {!t working_item_descr_entry} to a list of {!t
    t}*)
val of_working_item_descrs
  : Cobol_ptree.working_item_descr with_loc list
  -> t list Cobol_common.Diagnostics.with_diags
