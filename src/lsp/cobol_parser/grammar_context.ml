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
  | N_program_id_paragraph -> Some program_id_paragraph
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
  | 114 -> [options_paragraph]
  | 116 -> [intermediate_rounding_clause]
  | 128 -> [float_decimal_clause]
  | 141 -> [float_binary_clause]
  | 145 -> [entry_convention_clause]
  | 148 -> [rounded_phrase]
  | 154 -> [arithmetic_clause]
  | 193 -> [object_computer_paragraph]
  | 308 -> [dynlen_struct_clause]
  | 328 -> [currency_clause]
  | 366 -> [alphabet_clause]
  | 442 -> [interface_specifier]
  | 452 -> [function_specifier]
  | 459 -> [class_specifier]
  | 488 -> [sharing_clause]
  | 540 -> [lock_mode_clause]
  | 1083 -> [validate_status_clause]
  | 1109 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1114 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1116 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1117 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1130 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1131 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1132 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1135 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1136 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1137 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1138 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1141 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1143 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1148 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1150 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1152 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1153 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1154 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1155 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1156 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1157 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1158 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1159 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1160 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1161 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1162 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1163 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1164 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1165 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1166 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1167 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1173 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1175 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1177 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1179 -> [usage_clause   (* ok as none of leftmost terminals are C/S *)]
  | 1181 -> [typedef_clause]
  | 1381 -> [occurs_clause]
  | 1435 -> [typedef_clause]
  | 1461 -> [default_clause]
  | 1469 -> [constant]
  | 1936 -> [occurs_clause]
  | 1963 -> [line_clause                             (*NUMBERS only*)]
  | 1965 -> [line_clause                             (*NUMBERS only*)]
  | 1976 -> [column_clause              (* NUMBERS & CENTER *)]
  | 1982 -> [column_clause              (* NUMBERS & CENTER *)]
  | 1987 -> [column_clause              (* NUMBERS & CENTER *)]
  | 2009 -> [rounded_phrase]
  | 2078 -> [screen_descr_entry]
  | 2119 -> [erase_clause]
  | 2376 -> [retry_phrase]
  | 2470 -> [stop_stmt]
  | 2557 -> [set_stmt]
  | 2617 -> [set_attribute_stmt]
  | 2705 -> [resume_stmt]
  | 2723 -> [read_stmt]
  | 2782 -> [sharing_phrase]
  | 3027 -> [exit_stmt]
  | 3164 -> [allocate_stmt]
  | 3178 -> [accept_stmt]
  | 3906 -> [object_paragraph]
  | 3912 -> [factory_paragraph]
  | 3950 -> [program_id_paragraph]
  | 4023 -> [program_id_paragraph]
  | _ -> []

let contexts: type k. k lr1state -> _ list = fun s ->
  contexts_for_state_num (number s)

