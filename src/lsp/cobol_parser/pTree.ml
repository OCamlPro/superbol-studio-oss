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

type 'a with_loc = 'a Cobol_common.Srcloc.with_loc =
  { payload: 'a; loc: Cobol_common.Srcloc.srcloc; }
open Cobol_common.Srcloc.INFIX

(** Parse tree: raw AST with pictures represented using plain, unchecked
    strings. *)

module Misc_sections =
  Cobol_ast.Raw.Misc_sections
module Picture = struct
  type picture = string with_loc
  let pp_picture fmt str = Pretty.string fmt (~&str)
  let show_picture = Pretty.to_string "%a" pp_picture
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
