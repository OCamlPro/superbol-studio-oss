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

(** Parse tree: raw AST with pictures represented using plain, unchecked
    strings. *)

module Misc_sections =
  Misc_descr
module Picture_type = struct
  type picture = string with_loc
  let pp_picture fmt str = Pretty.string fmt (~&str)
  let show_picture = Pretty.to_string "%a" pp_picture
  let compare_picture = Cobol_common.Srcloc.compare_with_loc String.compare
end
module Data_sections =
  Raw.Data_sections (Picture_type)
module Data_division =
  Raw.Data_division (Data_sections)
module Statements =
  Raw.Statements
module Proc_division =
  Raw.Proc_division (Statements)
module Compilation_group =
  Raw.Compilation_group (Misc_sections) (Data_division) (Proc_division)

include Compilation_group
include Proc_division
include Statements
include Data_division
include Data_sections
include Picture_type
include Misc_sections

include Terms
include Data_descr
include Misc_descr
include Operands
include Simple_statements
include Branching_statements
