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
open Cobol_ir.Types
open Types

open Cobol_common.Srcloc.INFIX

(* --- *)

let create ~name ~source_file =

  let module_name = CArray.of_string name in
  let module_source = CArray.of_string source_file in
  let gc_version = Pretty.string_to CArray.of_string "tramabol-%s" Version.version in
  let module_formatted_date = CArray.of_string "December 31 9999 23:59:59" in
  let module_date = U32.of_int_unsafe 99991231 in
  let module_time = U32.of_int_unsafe 235959 in

  let pmodule = CPtr.create (CPtr (CComp CobModule.kind)) in
  let pglobals = CPtr.create (CPtr (CComp CobGlobal.kind)) in
  let _res = cob_module_global_enter pmodule pglobals S32.zero S32.zero in
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

  { module_ptr = CPtr.get pmodule; module_globals = CPtr.get pglobals }


let enter (m: module_handle) ~(params: cob_field array) =
  CobModule.set_cob_procedure_params m.module_memory.module_ptr
    (Field.cptr_of_array params);
  cob_set_cancel m.module_memory.module_ptr;
  CobModule.set_module_active m.module_memory.module_ptr
    (U32.succ_unsafe (CobModule.get_module_active m.module_memory.module_ptr))


let leave (m: module_handle) =
  CobModule.set_module_active m.module_memory.module_ptr
    (U32.pred_unsafe (CobModule.get_module_active m.module_memory.module_ptr));
  cob_module_leave m.module_memory.module_ptr


let cancel (m: module_handle) =                                    (* CHECKME *)
  cob_cancel CArray.(to_ptr @@ of_string ~&(~&(m.module_unit).unit_name))
