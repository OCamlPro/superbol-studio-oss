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

(** This module implements functions to type the COBOL data.*)

module Memory = Data_memory
module Types = Data_types
module Item = Data_item
module Picture = Data_picture
module Value = Data_value
module Literal = Data_literal
module Printer = Data_printer
module Visitor = Data_visitor

module Diagnostics = Data_diagnostics                         (* for literals *)
