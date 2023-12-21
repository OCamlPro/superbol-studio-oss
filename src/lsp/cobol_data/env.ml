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
open Types

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

module Names = Set.Make (struct
    type t = Cobol_ptree.name
    let compare = String.compare
  end)

module DATA_ITEM = struct
  type condition = {
    target: Cobol_ptree.qualname;
    values: Cobol_ptree.condition_name_value list;
  } [@@deriving show]

  type t =
    { name: Cobol_ptree.name;
      typ: data_type option;
      size: int;
      global: bool;
      value: Cobol_ptree.data_value_clause option;
      renames: Cobol_ptree.qualname list;
      condition: condition option;
      redefines: Cobol_ptree.qualname option;
      constant: Cobol_ptree.constant_value option; }
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
    { name: Cobol_ptree.name;
      parent_prog: t option;
      data_items: DATA_ITEM.t Qualmap.t;
      currency_signs: Cobol_common.Basics.CharSet.t;
      decimal_point: char;
      using_items: Names.t; }

  let make ?parent name =
    match parent with
    | None ->
        { name = name;
          parent_prog = None;
          data_items = Qualmap.empty;
          currency_signs = Cobol_common.Basics.CharSet.empty;
          decimal_point = '.';
          using_items = Names.empty }
    | Some parent ->
        { parent with
          name = name;
          parent_prog = Some parent;
          (* CHECKME: directly inherit those?  Even better: extract the
             non-environment-specific part into a `prog` structure, with a
             `prog_env` that is the (inherited and augmented) environment, and
             specific `prog_data`/`prog_using`. *)
          data_items = Qualmap.empty;
          using_items = Names.empty }

end

module ENV = struct
  include StringMap
end
