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
  | 154 -> [Options_paragraph]
  | 156 -> [Intermediate_rounding_clause]
  | 168 -> [Float_decimal_clause]
  | 181 -> [Float_binary_clause]
  | 185 -> [Entry_convention_clause]
  | 188 -> [Rounded_phrase]
  | 194 -> [Arithmetic_clause]
  | 233 -> [Object_computer_paragraph]
  | 347 -> [Dynlen_struct_clause]
  | 367 -> [Currency_clause]
  | 405 -> [Alphabet_clause]
  | 481 -> [Interface_specifier]
  | 491 -> [Function_specifier]
  | 498 -> [Class_specifier]
  | 527 -> [Sharing_clause]
  | 579 -> [Lock_mode_clause]
  | 1094 -> [Validate_status_clause]
  | 1120 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1125 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1127 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1128 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1141 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1142 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1143 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1146 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1147 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1148 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1149 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1152 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1154 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1159 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1161 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
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
  | 1174 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1175 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1176 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1177 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1178 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1184 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1186 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1188 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1190 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1192 -> [Usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1194 -> [Typedef_clause]
  | 1394 -> [Occurs_clause]
  | 1448 -> [Typedef_clause]
  | 1474 -> [Default_clause]
  | 1550 -> [Constant]
  | 1953 -> [Occurs_clause]
  | 1980 -> [Line_clause                             (*NUMBERS only*)]
  | 1982 -> [Line_clause                             (*NUMBERS only*)]
  | 1992 -> [Column_clause              (* NUMBERS & CENTER *)]
  | 1998 -> [Column_clause              (* NUMBERS & CENTER *)]
  | 2003 -> [Column_clause              (* NUMBERS & CENTER *)]
  | 2025 -> [Rounded_phrase]
  | 2095 -> [Screen_descr_entry]
  | 2136 -> [Erase_clause]
  | 2393 -> [Retry_phrase]
  | 2487 -> [Stop_stmt]
  | 2574 -> [Set_stmt]
  | 2634 -> [Set_attribute_stmt]
  | 2722 -> [Resume_stmt]
  | 2740 -> [Read_stmt]
  | 2799 -> [Sharing_phrase]
  | 3044 -> [Exit_stmt]
  | 3181 -> [Allocate_stmt]
  | 3195 -> [Accept_stmt]
  | 3910 -> [Program_id_paragraph]
  | 4040 -> [Object_paragraph]
  | 4050 -> [Factory_paragraph]
  | _ -> []

let contexts: type k. k lr1state -> _ list = fun s ->
  contexts_for_state_num (number s)

