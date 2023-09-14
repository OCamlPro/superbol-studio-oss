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

open Cobol_ast
open Types

module StringSet = Cobol_common.Basics.StringSet
module StringMap = Cobol_common.Basics.StringMap
module CharSet = Cobol_common.Basics.CharSet
module FATAL = Cobol_common.Diagnostics.Fatal

(*FIXME: Quite a bit of rework for c translation and analysis alike *)

(* For data item:
   * name
   * type
   * declaration loc
   * sub_items?
   * data_size
   * is global
   * default value
   * picture *)

(* For program:
    * name
    * nested_progs (just the name maybe?)
    * data_items
    * curr_signs (maybe char set)
    * decimal_point (maybe algebric)
    * entry arguments *)

module NameSet = Set.Make(struct
    type t = name
    let compare = String.compare
  end)

module Names = NameSet

module DATA_ITEM = struct
  type condition = {
    target: qualname;
    values: condition_name_value list;
  } [@@deriving show]

  type t =
    { name: name;
      typ: data_type option;
      size: int;
      global: bool;
      value: data_value_clause option;
      renames: qualname list;
      condition: condition option;
      redefines: qualname option;
      constant: constant_value option; }
  [@@deriving show]

  let make name =
    { name = name;
      typ = None;
      size = 0;
      global = false;
      value = None;
      renames = [];
      redefines = None;
      constant = None;
      condition = None; }
end

module PROG_ENV = struct
  type t =
    { name: name;
      parent_prog: t option;
      data_items: DATA_ITEM.t Qualmap.t;
      currency_signs: CharSet.t;
      decimal_point: char;
      using_items: NameSet.t; }

  let make ?parent name =
    match parent with
    | None ->
        { name = name;
          parent_prog = None;
          data_items = Qualmap.empty;
          currency_signs = CharSet.empty;
          decimal_point = '.';
          using_items = NameSet.empty }
    | Some parent ->
        { parent with
          name = name;
          parent_prog = Some parent;
          (* CHECKME: directly inherit those?  Even better: extract the
             non-environment-specific part into a `prog` structure, with a
             `prog_env` that is the (inherited and augmented) environment, and
             specific `prog_data`/`prog_using`. *)
          data_items = Qualmap.empty;
          using_items = NameSet.empty }

end

module ENV = struct
  include StringMap
end
