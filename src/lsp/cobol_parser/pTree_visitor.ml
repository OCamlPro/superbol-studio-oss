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

module For_misc_sections =
  Cobol_ast.Raw_visitor.Make_for_misc_sections

module For_picture = struct
  include Cobol_ast.Abstract_visitor.For_picture (PTree.Picture)
  let fold_picture (v: _ #folder) = Cobol_common.Visitor.leaf v#fold_picture
end

module For_data_sections =
  Cobol_ast.Raw_visitor.Make_for_data_sections (PTree.Picture)

module For_data_division =
  Cobol_ast.Raw_visitor.Make_for_data_division (PTree.Data_sections)

module For_statements =
  Cobol_ast.Raw_visitor.Make_for_statements

module For_proc_division =
  Cobol_ast.Raw_visitor.Make_for_proc_division (PTree.Statements)

module For_compilation_group =
  Cobol_ast.Raw_visitor.Make_for_compilation_group
    (PTree.Misc_sections) (PTree.Data_division) (PTree.Proc_division)

include For_picture
include For_data_sections
include For_data_division
include For_statements
include For_proc_division
include For_compilation_group

class ['a] folder = object (v)
  inherit ['a] For_misc_sections.folder
  inherit ['a] For_picture.folder
  inherit! ['a] For_data_sections.folder
  inherit! ['a] For_data_division.folder
  inherit! ['a] For_statements.folder
  inherit! ['a] For_proc_division.folder
  inherit! ['a] For_compilation_group.folder
  method private continue_with_informational_paragraphs =
    For_misc_sections.fold_informational_paragraphs v
  method private continue_with_options_paragraph =
    For_misc_sections.fold_options_paragraph v
  method private continue_with_environment_division =
    For_misc_sections.fold_environment_division v
  method private continue_with_picture =
    For_picture.fold_picture v
  method private continue_with_working_storage_section =
    For_data_sections.fold_working_storage_section v
  method private continue_with_screen_section =
    For_data_sections.fold_screen_section v
  method private continue_with_report_section =
    For_data_sections.fold_report_section v
  method private continue_with_local_storage_section =
    For_data_sections.fold_local_storage_section v
  method private continue_with_linkage_section =
    For_data_sections.fold_linkage_section v
  method private continue_with_file_section =
    For_data_sections.fold_file_section v
  method private continue_with_communication_section =
    For_data_sections.fold_communication_section v
  method private continue_with_data_division =
    For_data_division.fold_data_division v
  method private continue_with_statement' =
    For_statements.fold_statement' v
  method private continue_with_statements' =
    For_statements.fold_statements' v
  method private continue_with_procedure_division =
    For_proc_division.fold_procedure_division v
end
