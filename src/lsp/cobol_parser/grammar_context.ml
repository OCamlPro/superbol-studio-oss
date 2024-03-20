(* Caution: this file was automatically generated from grammar.cmly; do not edit *)
open Grammar
open MenhirInterpreter
open Grammar_contexts

let nonterminal_context: type k. k nonterminal -> _ option = function
  | N_validate_status_clause -> Some Validate_status_clause
  | N_usage -> Some Usage_clause   (* ok as none of leftmost terminals are C/S *)
  | N_typedef_clause -> Some Typedef_clause
  | N_stop_statement -> Some Stop_stmt
  | N_sharing_phrase -> Some Sharing_phrase
  | N_sharing_clause -> Some Sharing_clause
  | N_set_statement -> Some Set_stmt
  | N_set_attribute_switches -> Some Set_attribute_stmt
  | N_screen_descr_entry -> Some Screen_descr_entry
  | N_rounded_phrase -> Some Rounded_phrase
  | N_rounded_clause -> Some Rounded_phrase
  | N_retry_phrase -> Some Retry_phrase
  | N_resume_statement -> Some Resume_stmt
  | N_report_occurs_clause -> Some Occurs_clause
  | N_read_statement -> Some Read_stmt
  | N_program_definition_id_paragraph -> Some Program_id_paragraph
  | N_perform_statement -> Some Perform_stmt
  | N_options_paragraph -> Some Options_paragraph
  | N_occurs_fixed_clause -> Some Occurs_clause
  | N_occurs_dynamic_clause -> Some Occurs_clause
  | N_occurs_depending_clause -> Some Occurs_clause
  | N_object_paragraph -> Some Object_paragraph
  | N_object_computer_paragraph -> Some Object_computer_paragraph
  | N_lock_mode_clause -> Some Lock_mode_clause
  | N_line_header -> Some Line_clause                             (*NUMBERS only*)
  | N_intermediate_rounding_clause -> Some Intermediate_rounding_clause
  | N_interface_specifier -> Some Interface_specifier
  | N_function_specifier -> Some Function_specifier
  | N_float_decimal_clause -> Some Float_decimal_clause
  | N_float_binary_clause -> Some Float_binary_clause
  | N_factory_paragraph -> Some Factory_paragraph
  | N_exit_statement -> Some Exit_stmt
  | N_erase_clause -> Some Erase_clause
  | N_entry_convention_clause -> Some Entry_convention_clause
  | N_dynamic_length_structure_clause -> Some Dynlen_struct_clause
  | N_default_clause -> Some Default_clause
  | N_currency_sign_clause -> Some Currency_clause
  | N_constant_value_length -> Some Constant
  | N_column_header -> Some Column_clause              (* NUMBERS & CENTER *)
  | N_class_specifier -> Some Class_specifier
  | N_arithmetic_clause -> Some Arithmetic_clause
  | N_alphabet_name_clause -> Some Alphabet_clause
  | N_allocate_statement -> Some Allocate_stmt
  | N_accept_statement -> Some Accept_stmt
  | _ -> None

let contexts_for_state_num: int -> _ list = function
  | 28 -> [Program_id_paragraph]
  | 152 -> [Options_paragraph]
  | 154 -> [Intermediate_rounding_clause]
  | 166 -> [Float_decimal_clause]
  | 179 -> [Float_binary_clause]
  | 183 -> [Entry_convention_clause]
  | 186 -> [Rounded_phrase]
  | 192 -> [Arithmetic_clause]
  | 231 -> [Object_computer_paragraph]
  | 345 -> [Dynlen_struct_clause]
  | 365 -> [Currency_clause]
  | 402 -> [Alphabet_clause]
  | 478 -> [Interface_specifier]
  | 488 -> [Function_specifier]
  | 495 -> [Class_specifier]
  | 524 -> [Sharing_clause]
  | 575 -> [Lock_mode_clause]
  | 1089 -> [Validate_status_clause]
  | 1115 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1120 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1122 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1123 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1136 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1137 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1138 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1141 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1142 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1143 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1144 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1147 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1149 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1154 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1156 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1158 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1159 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1160 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1161 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1162 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1163 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1164 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1165 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1166 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1167 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1168 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1169 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1170 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1171 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1172 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1173 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1179 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1181 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1183 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1185 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1187 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1189 -> [Typedef_clause]
  | 1389 -> [Occurs_clause]
  | 1443 -> [Typedef_clause]
  | 1469 -> [Default_clause]
  | 1545 -> [Constant]
  | 1980 -> [Occurs_clause]
  | 2007 -> [Line_clause                             (*NUMBERS only*)]
  | 2009 -> [Line_clause                             (*NUMBERS only*)]
  | 2019 -> [Column_clause              (* NUMBERS & CENTER *)]
  | 2025 -> [Column_clause              (* NUMBERS & CENTER *)]
  | 2030 -> [Column_clause              (* NUMBERS & CENTER *)]
  | 2052 -> [Rounded_phrase]
  | 2122 -> [Screen_descr_entry]
  | 2163 -> [Erase_clause]
  | 2425 -> [Retry_phrase]
  | 2519 -> [Stop_stmt]
  | 2604 -> [Set_stmt]
  | 2664 -> [Set_attribute_stmt]
  | 2752 -> [Resume_stmt]
  | 2770 -> [Read_stmt]
  | 2797 -> [Perform_stmt]
  | 2830 -> [Sharing_phrase]
  | 3074 -> [Exit_stmt]
  | 3210 -> [Allocate_stmt]
  | 3224 -> [Accept_stmt]
  | 3930 -> [Program_id_paragraph]
  | 4049 -> [Object_paragraph]
  | 4059 -> [Factory_paragraph]
  | _ -> []

let contexts: type k. k lr1state -> _ list = fun s ->
  contexts_for_state_num (number s)

