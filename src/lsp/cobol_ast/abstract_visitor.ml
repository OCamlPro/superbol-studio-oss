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

(** Generic visitors for selected AST abstractions *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Visitor

module For_misc_sections (Misc_sections: Abstract.MISC_SECTIONS) = struct
  open Misc_sections
  class virtual ['a] folder = object
    method fold_informational_paragraphs: (informational_paragraphs, 'a) fold = default
    method fold_options_paragraph: (options_paragraph, 'a) fold = default
    method fold_environment_division: (environment_division, 'a) fold = default
    method virtual continue_with_informational_paragraphs: informational_paragraphs -> 'a -> 'a
    method virtual continue_with_options_paragraph: options_paragraph -> 'a -> 'a
    method virtual continue_with_environment_division: environment_division -> 'a -> 'a
  end
end

module For_picture (Picture: Abstract.PICTURE) = struct
  open Picture
  class virtual ['a] folder = object
    method fold_picture: (picture, 'a) fold = default
    method virtual continue_with_picture: picture -> 'a -> 'a
  end
end

module For_data_sections (Data_sections: Abstract.DATA_SECTIONS) = struct
  open Data_sections
  class virtual ['a] folder = object
    method fold_working_storage_section: (working_storage_section, 'a) fold = default
    method fold_linkage_section        : (linkage_section        , 'a) fold = default
    method fold_file_section           : (file_section           , 'a) fold = default
    method fold_communication_section  : (communication_section  , 'a) fold = default
    method fold_local_storage_section  : (local_storage_section  , 'a) fold = default
    method fold_report_section         : (report_section         , 'a) fold = default
    method fold_screen_section         : (screen_section         , 'a) fold = default
    method virtual continue_with_working_storage_section: working_storage_section -> 'a -> 'a
    method virtual continue_with_linkage_section        : linkage_section         -> 'a -> 'a
    method virtual continue_with_file_section           : file_section            -> 'a -> 'a
    method virtual continue_with_communication_section  : communication_section   -> 'a -> 'a
    method virtual continue_with_local_storage_section  : local_storage_section   -> 'a -> 'a
    method virtual continue_with_report_section         : report_section          -> 'a -> 'a
    method virtual continue_with_screen_section         : screen_section          -> 'a -> 'a
  end
end

module For_data_division (Data_division: Abstract.DATA_DIVISION) = struct
  open Data_division
  class virtual ['a] folder = object
    method fold_data_division: (data_division, 'a) fold = default
    method virtual continue_with_data_division: data_division -> 'a -> 'a
  end
end

module For_statements (Statements: Abstract.STATEMENTS) = struct
  open Statements
  class virtual ['a] folder = object
    method fold_statement': (statement with_loc, 'a) fold = default
    method fold_statements': (statements with_loc, 'a) fold = default
    method virtual continue_with_statement': statement with_loc -> 'a -> 'a
    method virtual continue_with_statements': statements with_loc -> 'a -> 'a
  end
end

module For_proc_division (Proc_division: Abstract.PROC_DIVISION) = struct
  open Proc_division
  class virtual ['a] folder = object
    method fold_procedure_division: (procedure_division, 'a) fold = default
    method virtual continue_with_procedure_division: procedure_division -> 'a -> 'a
  end
end

module For_compilation_group (Compilation_group: Abstract.COMPILATION_GROUP) = struct
  open Compilation_group
  class virtual ['a] folder = object
    method fold_compilation_group: (compilation_group, 'a) fold = default
    (* method virtual continue_with_compilation_group : compilation_group -> 'a -> 'a *)
  end
end
