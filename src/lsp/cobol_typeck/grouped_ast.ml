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

module Misc_sections =
  Cobol_ast.Raw.Misc_sections
module Data_sections = struct
  include Cobol_data.Pictured_ast.Data_sections
  type working_storage_section = Cobol_data.Group.t list [@@deriving show, ord]
  type linkage_section = Cobol_data.Group.t list [@@deriving show, ord]
end
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
include Cobol_data.Pictured_ast.Picture
include Cobol_data.Pictured_ast.Data_sections
include Cobol_data.Pictured_ast.Misc_sections

(*
module Data_div_mapper = struct
  open Pictured_ast.Data_div
  let data_entries = List.filter_map begin function
      | Pictured_ast.Data_section.{ payload = Data d; _ } as cdde ->
          Some (d &@<- cdde)
      | _ -> None
    end
  let file_section (fs: file_section) = (fs: Data_div_repr.file_section)
  (* TODO: Replace module here *)
  let working_storage_section (wss: working_storage_section) =
    Result.get_ok @@ DataGroup.of_working_storage (module Cobol_common.Diagnostics.InitStateful ()) wss
  let linkage_section ls =
    Result.get_ok @@ DataGroup.of_working_storage (module Cobol_common.Diagnostics.InitStateful ()) ls
  let communication_section = Fun.id
  let local_storage_section = Fun.id
  let report_section = Fun.id
  let screen_section = Fun.id
end

module Data_div_traversal = Traversal.Make_data_div_traversal
    (Pictured_ast.Data_div) (Grouped_data_div_ast) (Data_div_mapper)

module Pictured_to_grouped_traversal =
  Traversal.Make (Pictured_ast) (Grouped_ast) (struct
    let picture = Fun.id
    include Data_div_mapper
  end)

module Grouped_ast_traversal = Traversal.MakeId (Grouped_ast)
*)
