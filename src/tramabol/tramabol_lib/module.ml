(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezlibcob.V1

let create ~name ~source_file : Types.module_ =

  let module_name = CArray.of_string name in
  let module_source = CArray.of_string source_file in
  let gc_version = CArray.of_string "3.3-dev" in
  let module_formatted_date = CArray.of_string "juin 17 2026 23:13:30" in
  let module_date = U32.of_int 20260617 in
  let module_time = U32.of_int 231330 in

  let pmodule = CPtr.create CComp in
  let module_ = CPtr.get pmodule in

  let pcob_module_path = CPtr.create ~default:(CPtr.null Char) (CPtr Char) in

  CobModule.set_module_name module_ (CArray.to_ptr module_name);
  CobModule.set_module_formatted_date module_ (CArray.to_ptr module_formatted_date);
  CobModule.set_module_source module_ (CArray.to_ptr module_source);
  CobModule.set_gc_version module_ (CArray.to_ptr gc_version);
(* CobModule.set_module_entry module_ *)
(* CobModule.set_module_cancel module _ *)
  CobModule.set_module_ref_count module_ (CPtr.null UInt32);
  CobModule.set_module_path module_ pcob_module_path;
  CobModule.set_module_active module_ U32.zero;
  CobModule.set_module_date module_ module_date;
  CobModule.set_module_time module_ module_time;
  CobModule.set_module_type module_ U32.zero;
  CobModule.set_module_param_cnt module_ U32.zero;
  CobModule.set_ebcdic_sign module_ U8.zero;
  CobModule.set_decimal_point module_ (U8.of_char '.');
  CobModule.set_currency_symbol module_ (U8.of_char '$');
  CobModule.set_numeric_separator module_ (U8.of_char ',');
  CobModule.set_flag_filename_mapping module_ U8.one;
  CobModule.set_flag_binary_truncate module_ U8.one;
  CobModule.set_flag_pretty_display module_ U8.one;
  CobModule.set_flag_host_sign module_ U8.zero;
  CobModule.set_flag_no_phys_canc module_ U8.one;
  CobModule.set_flag_main module_ U8.one;
  CobModule.set_flag_fold_call module_ U8.zero;
  CobModule.set_flag_exit_program module_ U8.zero;
  CobModule.set_flag_debug_trace module_ U8.zero;
  CobModule.set_flag_dump_ready module_ U8.zero;
  CobModule.set_xml_mode module_ U8.one;
  CobModule.set_module_stmt module_ U32.zero;
  CobModule.set_module_sources module_ (CPtr.null (CPtr Char));

  CobModule.set_collating_sequence module_ (CPtr.null UInt8);
  CobModule.set_crt_status module_ (CobField.null ());
  CobModule.set_cursor_pos module_ (CobField.null ());
  CobModule.set_xml_code module_ (CobField.null ());
  CobModule.set_xml_event module_ (CobField.null ());
  CobModule.set_xml_information module_ (CobField.null ());
  CobModule.set_xml_namespace module_ (CobField.null ());
  CobModule.set_xml_namespace_prefix module_ (CobField.null ());
  CobModule.set_xml_nnamespace module_ (CobField.null ());
  CobModule.set_xml_nnamespace_prefix module_ (CobField.null ());
  CobModule.set_xml_ntext module_ (CobField.null ());
  CobModule.set_xml_text module_ (CobField.null ());
  CobModule.set_json_code module_ (CobField.null ());
  CobModule.set_json_status module_ (CobField.null ());

  cob_set_cancel module_;

  pmodule

let enter (pmodule: Types.module_) ~(procedure_params: Types.fields_array) =
  let pglobal = CPtr.create CComp in
  let _res = cob_module_global_enter pmodule pglobal S32.zero S32.zero in
  let module_ = CPtr.get pmodule in
  let _global = CPtr.get pglobal in

  (* let cob_procedure_params = CArray.create ~default:(CobField.null ()) CComp 1 in *)

  CobModule.set_cob_procedure_params module_ (CArray.to_ptr procedure_params);

  cob_set_cancel module_;

  CobModule.set_module_active module_
    (U32.succ (CobModule.get_module_active module_));

  pglobal

let leave (pmodule: Types.module_) =
  let module_ = CPtr.get pmodule in

  CobModule.set_module_active module_
    (U32.pred (CobModule.get_module_active module_));

  cob_module_leave module_
