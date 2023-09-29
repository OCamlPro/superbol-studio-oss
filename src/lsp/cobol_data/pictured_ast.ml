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

(* NB: not sure we need the full pictured AST. We may only need (part of)
   Data_sections/division. *)

open Cobol_ast

module CharSet = Cobol_common.Basics.CharSet

module Misc_sections =
  Cobol_ast.Raw.Misc_sections
module Picture = struct
  type picture = Picture.t with_loc [@@deriving show, ord]
end
module Data_sections =
  Cobol_ast.Raw.Data_sections (Picture)
module Data_division =
  Cobol_ast.Raw.Data_division (Data_sections)
module Statements =
  Cobol_ast.Raw.Statements
module Proc_division =
  Cobol_ast.Raw.Proc_division (Statements)
module Compilation_group =
  Cobol_ast.Raw.Compilation_group
    (Misc_sections) (Data_division) (Proc_division)

include Compilation_group
include Proc_division
include Statements
include Data_division
include Data_sections
include Picture
include Misc_sections
