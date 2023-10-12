(* Caution: this file was automatically generated from grammar.cmly; do not edit *)
open Grammar
open MenhirInterpreter
open Grammar_contexts

let nonterminal_context: type k. k nonterminal -> _ option = function
  | N_validate_status_clause -> Some validate_status_clause
  | N_usage -> Some usage_clause   (* ok as none of leftmost terminals are C/S *)
  | N_typedef_clause -> Some typedef_clause
  | N_stop_statement -> Some stop_stmt
  | N_sharing_phrase -> Some sharing_phrase
  | N_sharing_clause -> Some sharing_clause
  | N_set_statement -> Some set_stmt
  | N_set_attribute_switches -> Some set_attribute_stmt
  | N_screen_descr_entry -> Some screen_descr_entry
  | N_rounded_phrase -> Some rounded_phrase
  | N_rounded_clause -> Some rounded_phrase
  | N_retry_phrase -> Some retry_phrase
  | N_resume_statement -> Some resume_stmt
  | N_report_occurs_clause -> Some occurs_clause
  | N_read_statement -> Some read_stmt
  | N_program_definition_id_paragraph -> Some program_id_paragraph
  | N_options_paragraph -> Some options_paragraph
  | N_occurs_fixed_clause -> Some occurs_clause
  | N_occurs_dynamic_clause -> Some occurs_clause
  | N_occurs_depending_clause -> Some occurs_clause
  | N_object_paragraph -> Some object_paragraph
  | N_object_computer_paragraph -> Some object_computer_paragraph
  | N_lock_mode_clause -> Some lock_mode_clause
  | N_line_header -> Some line_clause                             (*NUMBERS only*)
  | N_intermediate_rounding_clause -> Some intermediate_rounding_clause
  | N_interface_specifier -> Some interface_specifier
  | N_function_specifier -> Some function_specifier
  | N_float_decimal_clause -> Some float_decimal_clause
  | N_float_binary_clause -> Some float_binary_clause
  | N_factory_paragraph -> Some factory_paragraph
  | N_exit_statement -> Some exit_stmt
  | N_erase_clause -> Some erase_clause
  | N_entry_convention_clause -> Some entry_convention_clause
  | N_dynamic_length_structure_clause -> Some dynlen_struct_clause
  | N_default_clause -> Some default_clause
  | N_currency_sign_clause -> Some currency_clause
  | N_constant_value_length -> Some constant
  | N_column_header -> Some column_clause              (* NUMBERS & CENTER *)
  | N_class_specifier -> Some class_specifier
  | N_arithmetic_clause -> Some arithmetic_clause
  | N_alphabet_name_clause -> Some alphabet_clause
  | N_allocate_statement -> Some allocate_stmt
  | N_accept_statement -> Some accept_stmt
  | _ -> None

let contexts_for_state_num: int -> _ list = function
  | 28 -> [program_id_paragraph]
  | 154 -> [options_paragraph]
  | 156 -> [intermediate_rounding_clause]
  | 168 -> [float_decimal_clause]
  | 181 -> [float_binary_clause]
  | 185 -> [entry_convention_clause]
  | 188 -> [rounded_phrase]
  | 194 -> [arithmetic_clause]
  | 233 -> [object_computer_paragraph]
  | 347 -> [dynlen_struct_clause]
  | 367 -> [currency_clause]
  | 405 -> [alphabet_clause]
  | 481 -> [interface_specifier]
  | 491 -> [function_specifier]
  | 498 -> [class_specifier]
  | 527 -> [sharing_clause]
  | 579 -> [lock_mode_clause]
  | 1094 -> [validate_status_clause]
  | 1120 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1125 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1127 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1128 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1141 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1142 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1143 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1146 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1147 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1148 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1149 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1152 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1154 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1159 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1161 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1163 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1164 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1165 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1166 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1167 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1168 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1169 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1170 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1171 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1172 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1173 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1174 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1175 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1176 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1177 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1178 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1184 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1186 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1188 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1190 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1192 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1194 -> [typedef_clause]
  | 1394 -> [occurs_clause]
  | 1448 -> [typedef_clause]
  | 1474 -> [default_clause]
  | 1479 -> [constant]
  | 1953 -> [occurs_clause]
  | 1980 -> [line_clause                             (*NUMBERS only*)]
  | 1982 -> [line_clause                             (*NUMBERS only*)]
  | 1992 -> [column_clause              (* NUMBERS & CENTER *)]
  | 1998 -> [column_clause              (* NUMBERS & CENTER *)]
  | 2003 -> [column_clause              (* NUMBERS & CENTER *)]
  | 2025 -> [rounded_phrase]
  | 2094 -> [screen_descr_entry]
  | 2135 -> [erase_clause]
  | 2392 -> [retry_phrase]
  | 2486 -> [stop_stmt]
  | 2573 -> [set_stmt]
  | 2633 -> [set_attribute_stmt]
  | 2721 -> [resume_stmt]
  | 2739 -> [read_stmt]
  | 2798 -> [sharing_phrase]
  | 3043 -> [exit_stmt]
  | 3180 -> [allocate_stmt]
  | 3194 -> [accept_stmt]
  | 3909 -> [program_id_paragraph]
  | 4039 -> [object_paragraph]
  | 4049 -> [factory_paragraph]
  | _ -> []

let contexts: type k. k lr1state -> _ list = fun s ->
  contexts_for_state_num (number s)

