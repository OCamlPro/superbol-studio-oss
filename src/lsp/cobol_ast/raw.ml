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

open Ast

module Misc_sections = struct
  type informational_paragraphs = Ast.informational_paragraphs [@@deriving show]
  type options_paragraph        = Ast.options_paragraph        [@@deriving show]
  type environment_division     = Ast.environment_division     [@@deriving show]
end

module Data_sections (Picture: Abstract.PICTURE) = struct
  include Picture

  type picture_clause =
    {
      picture: Picture.picture;
      picture_locale: locale_phrase option;
      picture_depending: qualname with_loc option;
    }
  [@@deriving show]

  (* TODO (can be delayed): Unify some of the types (probably like for terms) *)

  type data_clause =
    | DataAligned                                                 (* +COB2002 *)
    | DataAnyLength                                               (* +COB2002 *)
    | DataBased                                                   (* +COB2002 *)
    | DataBlankWhenZero
    | DataConstantRecord                                          (* +COB2002 *)
    | DataOccurs of data_occurs_clause
    | DataType of name with_loc                                   (* +COB2002 *)
    | DataValue of data_value_clause
    | DataDynamicLength of                                        (* +COB2002 *)
        {
          dynamic_length_structure_name: name with_loc option;
          limit_is: integer option;
        }
    | DataExternal of external_clause
    | DataGlobal
    | DataGroupUsage of group_usage_clause                        (* +COB2002 *)
    | DataJustified
    | DataPicture of picture_clause with_loc
    | DataProperty of property_clause with_loc                    (* +COB2002 *)
    | DataRedefines of name with_loc
    | DataSameAs of name with_loc                                 (* +COB2002 *)
    | DataSelectWhen of select_when_clause                        (* +COB2002 *)
    | DataSign of sign_clause
    | DataSynchronized of synchronized_clause
    | DataTypedef of                                              (* +COB2002 *)
        {
          strong: bool;
        }
    | DataUsage of usage_clause
    | DataValidation of validation_clause                         (* +COB2002 *)
  [@@deriving show]

  type report_group_clause =
    | ReportType of report_type_clause
    | ReportNextGroup of next_group_clause
    | ReportLine of line_position list
    | ReportPicture of picture_clause with_loc
    | ReportUsage of report_screen_usage_clause
    | ReportSign of sign_clause
    | ReportJustified
    | ReportColumn of
        {
          alignment: alignment;
          position: column_position list;                        (* non-empty *)
        }
    | ReportBlankWhenZero
    | ReportSource of
        {
          source: expression list;                               (* non-empty *)
          rounding: rounding;
        }
    | ReportSum of
        {
          sum_of: sum_phrase list;                               (* non-empty *)
          reset_on: report_data_name_or_final option;
          rounding: rounding;
        }
    | ReportValue of literal
    | ReportPresentWhen of condition                              (* +COB2002 *)
    | ReportGroupIndicate
    | ReportOccurs of                                             (* +COB2002 *)
       {
         from: integer;
         to_: integer option;
         depending: qualname with_loc option;
         step: integer option;
       }
    | ReportVarying of data_varying list                          (* +COB2002 *)
  [@@deriving show]

  type screen_clause =
    | ScreenAuto
    | ScreenBlank of blank_clause
    | ScreenBlankWhenZero
    | ScreenErase of erase_clause
    | ScreenFull
    | ScreenGlobal
    | ScreenJustified
    | ScreenPicture of picture_clause with_loc
    | ScreenUsage of report_screen_usage_clause
    | ScreenRequired
    | ScreenAttribute of screen_attribute_clause with_loc list   (* non-empty *)
    | ScreenColumn of screen_line_column_clause
    | ScreenLine of screen_line_column_clause
    | ScreenOccurs of integer
    | ScreenSecure
    | ScreenSign of sign_clause
    | ScreenSourceDestination of source_destination_clause with_loc list (* non-empty *)
  [@@deriving show]

  type data_item =
    {
      data_level: data_level with_loc;
      data_name: data_name with_loc option;
      data_clauses: data_clause with_loc list;
    }
  [@@deriving show]

  type screen_item =
    {
      screen_level: int;
      screen_data_name: data_name with_loc option;
      screen_clauses: screen_clause with_loc list;
    }
  [@@deriving show]

  type report_group_item =
    {
      report_level: int;
      report_data_name: data_name with_loc option;
      report_group_clauses: report_group_clause with_loc list;
    }
  [@@deriving show]

  type data_ = [`data]
  type constant_ = [`const]
  type rename_ = [`rename]
  type condition_name_ = [`condition_name]
  type screen_ = [`screen]
  type report_group_ = [`report_group]
  type _ item_descr =
    | Constant: constant_item -> [>constant_] item_descr
    | Data: data_item -> [>data_] item_descr
    | Renames: rename_item -> [>rename_] item_descr
    | CondName: condition_name_item -> [>condition_name_] item_descr
    | Screen: screen_item -> [>screen_] item_descr
    | ReportGroup: report_group_item -> [>report_group_] item_descr

  let pp_item_descr (type k) : k item_descr Pretty.printer = fun ppf -> function
    | Constant c -> pp_constant_item ppf c
    | Data d -> pp_data_item ppf d
    | Renames r -> pp_rename_item ppf r
    | CondName c -> pp_condition_name_item ppf c
    | Screen s -> pp_screen_item ppf s
    | ReportGroup r -> pp_report_group_item ppf r

  type data_item_descr    = data_ item_descr
  and constant_item_descr = constant_ item_descr
  and working_item_descr  = [constant_|data_|
                             rename_|condition_name_] item_descr
  and report_item_descr   = [constant_|report_group_] item_descr
  and screen_item_descr   = [constant_|screen_]       item_descr
  and any_item_descr      = [constant_|data_|rename_|condition_name_|
                             report_group_|screen_] item_descr
  let pp_data_item_descr     = pp_item_descr
  let pp_constant_item_descr = pp_item_descr
  let pp_working_item_descr  = pp_item_descr
  let pp_report_item_descr   = pp_item_descr
  let pp_screen_item_descr   = pp_item_descr
  let pp_any_item_descr      = pp_item_descr

  type working_storage_item_descr = working_item_descr [@@deriving show]
  type linkage_item_descr         = working_item_descr [@@deriving show]
  type file_item_descr            = working_item_descr [@@deriving show]
  type communication_item_descr   = working_item_descr [@@deriving show]
  type local_storage_item_descr   = working_item_descr [@@deriving show]

  type file_descr =
    {
      file_name: name with_loc;
      file_clauses: file_clauses;
      file_items: file_item_descr with_loc list;
    }
  [@@deriving show]

  and file_clauses =
    | FileFD of file_fd_clause with_loc list
    | FileSD of file_sd_clause with_loc list
  [@@deriving show]

  and file_fd_clause =
    | FileExternal of external_clause
    | FileGlobal
    | FileFormat of format_clause
    | FileBlockContains of
        {
          from: integer;
          to_: integer option;
          characters_or_records: file_block_contents;
        }
    | FileRecord of record_clause
    | FileLabel of label_clause
    | FileValueOf of value_of_clause list
    | FileData of file_data_clause
    | FileLinage of file_linage_clause
    | FileCodeSet of alphabet_specification
    | FileReport of name with_loc list
  [@@deriving show]

  and file_sd_clause =
    | FileSDRecord of record_clause
    | FileSDData of file_data_clause
    | FileSDGlobal
  [@@deriving show]


  type communication_descr =
    {
      comm_name: name with_loc;
      comm_clauses: comm_clause with_loc list;
      comm_items: communication_item_descr with_loc list;
      comm_direction: comm_direction;
    }
  [@@deriving show]

  and comm_direction =
    | CommInput of { initial: bool; items: data_name with_loc list }
    | CommOutput
    | CommIO of { initial: bool; items: name with_loc list }
  [@@deriving show]


  type report_descr =
    {
      report_name: name with_loc;
      report_clauses: report_clause with_loc list;
      report_items: report_item_descr with_loc list;
    }
  [@@deriving show]


  (* Actual sections *)

  type file_section = file_descr with_loc list [@@deriving show]
  type working_storage_section = working_storage_item_descr with_loc list [@@deriving show]
  type linkage_section = linkage_item_descr with_loc list [@@deriving show]
  type communication_section = communication_descr with_loc list [@@deriving show]
  type local_storage_section = local_storage_item_descr with_loc list [@@deriving show]
  type report_section = report_descr with_loc list [@@deriving show]
  type screen_section = screen_item_descr with_loc list [@@deriving show]

end

module Data_division (Data_sections: Abstract.DATA_SECTIONS) = struct
  include Data_sections

  type data_division =
    {
      file_section: file_section option;
      working_storage_section: working_storage_section option;
      linkage_section: linkage_section option;
      communication_section: communication_section option;
      local_storage_section: local_storage_section option;
      report_section: report_section option;
      screen_section: screen_section option;
    }
  [@@deriving show]

end

module Statements = struct
  type statement  = Ast.statement  [@@deriving show]
  type statements = Ast.statements [@@deriving show]
end

module Proc_division (Statements: Abstract.STATEMENTS) = struct
  include Statements

  type procedure_division =
    {
      procedure_using_clauses: using_clause with_loc list;
      procedure_returning: ident with_loc option;
      procedure_raising_phrases: raising_phrase with_loc list;
      procedure_declaratives: declarative with_loc list;
      procedure_paragraphs: paragraph with_loc list;
    }
  [@@deriving show]

  and using_clause =
    | UsingByReference of using_by_reference list
    | UsingByValue of name with_loc list
  [@@deriving show]

  and using_by_reference =
    {
      using_by_reference: name with_loc;
      using_by_reference_optional: bool;
    }
  [@@deriving show]

  and raising_phrase =
    {
      raising: name with_loc;
      raising_factory: bool;
    }
  [@@deriving show]

  and declarative =
    {
      declarative_name: name with_loc;
      declarative_segment: integer option;
      declarative_use: declarative_use option;
      declarative_sentences: statements with_loc list;
    }
  [@@deriving show]

  and declarative_use =
    | UseAfterFileException of
        {
          global: bool;
          trigger: use_file_exception_on;
        }
    | UseBeforeReporting of
        {
          global: bool;
          report_group: ident;
        }
    | UseForDebugging of use_for_debugging_target list
    | UseAfterIOException of use_after_exception list
    | UseAfterExceptionObject of name with_loc
  [@@deriving show]

  and use_for_debugging_target =
    | UseForDebuggingProcedure of
        {
          all: bool;
          procedure: qualname;
        }
    | UseForDebuggingAllProcedures
  [@@deriving show]

  and use_file_exception_on =
    | UseFileExceptionOnNames of name with_loc list
    | UseFileExceptionOnOpenMode of open_mode
  [@@deriving show]

  and use_after_exception =
    {
      use_after_exception: name with_loc;
      use_after_exception_on_files: name with_loc list;
    }
  [@@deriving show]

  and paragraph =
    {
      paragraph_name: name with_loc option;
      paragraph_is_section: bool;
      paragraph_segment: integer option;
      paragraph_sentences: statements with_loc list;
    }
  [@@deriving show]

end

module Compilation_group
    (Misc_sections: Abstract.MISC_SECTIONS)
    (Data_division: Abstract.DATA_DIVISION)
    (Proc_division: Abstract.PROC_DIVISION) =
struct

  include Misc_sections
  include Data_division
  include Proc_division

  type program_unit =
    {
      program_name: name with_loc;
      program_as: strlit option;
      program_level: program_level;
      program_options: options_paragraph option;
      program_env: environment_division with_loc option;
      program_data: data_division with_loc option;
      program_proc: procedure_division with_loc option;
      program_end_name: name with_loc option;
    }
  [@@deriving show]

  and program_level =
    | ProgramDefinition of
        {       (* Note: more general than before (allows nested prototypes): *)
          kind: program_kind option;
          has_identification_division: bool;
          informational_paragraphs: informational_paragraphs; (* ~COB85, -COB2002 *)
          nested_programs: program_unit with_loc list;
        }
    | ProgramPrototype
  [@@deriving show]

  and program_kind =
    | Common
    | Initial
    | Recursive
  [@@deriving show]

  type function_unit =
    {
      function_name: name with_loc;
      function_as: strlit option;
      function_is_proto: bool;
      function_options: options_paragraph option;
      function_env: environment_division with_loc option;
      function_data: data_division with_loc option;
      function_proc: procedure_division option;
      function_end_name: name with_loc;
    }
  [@@deriving show]

  type method_definition =
    {
      method_name: name with_loc;
      method_kind: method_kind;
      method_override: bool;
      method_final: bool;
      method_options: options_paragraph option;
      method_env: environment_division with_loc option;
      method_data: data_division with_loc option;
      method_proc: procedure_division option;
      method_end_name: name with_loc;
    }
  [@@deriving show]

  and method_kind =
    | NamedMethod of { as_: strlit option }
    | PropertyMethod of { kind: property_kind }

  type factory_definition = (* Note: could be merged with instance_definition *)
    {
      factory_implements: name with_loc list;
      factory_options: options_paragraph option;
      factory_env: environment_division with_loc option;
      factory_data: data_division with_loc option;
      factory_methods: method_definition with_loc list option;
    }
  [@@deriving show]

  type instance_definition =
    {
      instance_implements: name with_loc list;
      instance_options: options_paragraph option;
      instance_env: environment_division with_loc option;
      instance_data: data_division with_loc option;
      instance_methods: method_definition with_loc list option;
    }
  [@@deriving show]

  type class_definition =
    {
      class_name: name with_loc;
      class_as: strlit option;
      class_final: bool;
      class_inherits: name with_loc list;
      class_usings: name with_loc list;
      class_options: options_paragraph option;
      class_env: environment_division with_loc option;
      class_factory: factory_definition option;
      class_instance: instance_definition option;
      class_end_name: name with_loc;
    }
  [@@deriving show]

  type interface_definition =
    {
      interface_name: name with_loc;
      interface_as: strlit option;
      interface_inherits: name with_loc list;
      interface_usings: name with_loc list;
      interface_options: options_paragraph option;
      interface_env: environment_division with_loc option;
      interface_methods: method_definition with_loc list option;
      interface_end_name: name with_loc;
    }
  [@@deriving show]

  type compilation_unit =
    | Program of program_unit
    | Function of function_unit
    | ClassDefinition of class_definition
    | InterfaceDefinition of interface_definition
  [@@deriving show]

  type compilation_group =
    compilation_unit with_loc list
  [@@deriving show]

end
