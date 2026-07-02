(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezlibcob.V1

(*
let ignore_int (_n : int) = ()

let run () =
  let cob_module = LIBCOB.module_create ~name:"test" in
  let cob_context = LIBCOB.module_enter cob_module in

  let a_1 = LIBCOB.field_attr_create COB_TYPE_NUMERIC ~digits:3 in
  let a_2 = LIBCOB.field_attr_create COB_TYPE_ALPHANUMERIC
      ~flags:[ COB_FLAG_CONSTANT ] in
  let a_3 = LIBCOB.field_attr_create COB_TYPE_GROUP in
  let a_4 = LIBCOB.field_attr_create COB_TYPE_ALPHANUMERIC in

  let constant_buf = LIBCOB.buffer_create 24 in
  let c_1 = LIBCOB.field_create ~size:7 constant_buf ~buf_pos:0 a_2 in
  let c_2 = LIBCOB.field_create ~size:7 constant_buf ~buf_pos:8 a_2 in
  let c_3 = LIBCOB.field_create ~size:7 constant_buf ~buf_pos:16 a_2 in
  LIBCOB.field_init c_1 "var1 = ";
  LIBCOB.field_init c_2 "var2 = ";
  LIBCOB.field_init c_3 "var3 = ";

  let working_buf = LIBCOB.buffer_create 24 in
  let f_17 = LIBCOB.field_create ~size:6 working_buf ~buf_pos:0 a_3 in
  let f_18 = LIBCOB.field_create ~size:3 working_buf ~buf_pos:0 a_1 in
  let f_19 = LIBCOB.field_create ~size:3 working_buf ~buf_pos:3 a_1 in
  let f_20 = LIBCOB.field_create ~size:6 working_buf ~buf_pos:8 a_4 in
  let f_21 = LIBCOB.field_create ~size:3 working_buf ~buf_pos:16 a_1 in

  if cob_context.need_module_init then begin
    LIBCOB.field_init f_17 "000000";
    LIBCOB.field_init f_20 "      ";
    LIBCOB.field_init f_21 "000";
    ()
  end;

  LIBCOB.field_init f_18 "888";
  LIBCOB.display [| c_1 ; f_17 |];
  LIBCOB.field_init f_19 "999";
  LIBCOB.display [| c_1 ; f_17 |];

  LIBCOB.display [| LIBCOB.field_of_string "var3 = var1.left + var1.right" |];
  LIBCOB.decimal_set_field cob_context 0 ~src:f_18;
  LIBCOB.decimal_set_field cob_context 1 ~src:f_19;
  LIBCOB.decimal_add cob_context 0 1;
  ignore_int @@ LIBCOB.decimal_get_field cob_context 0 ~dst:f_21 ~flags:0;

  LIBCOB.display [| LIBCOB.field_of_string "memcpy var1 to var2" |];
  LIBCOB.memcpy ~src:f_17 ~dst:f_20 6;
  LIBCOB.display [| c_1 ; f_17 |];
  LIBCOB.display [| c_2 ; f_20 |];
  LIBCOB.display [| c_3 ; f_21 |];

  LIBCOB.module_leave cob_context

let () =
  LIBCOB.init [| "main" |];
  run ();
  LIBCOB.stop_run ~status: 0
*)



let run () =

  let module_name = CArray.of_string "test" in
  let module_source = CArray.of_string "test.cob" in
  let gc_version = CArray.of_string "3.3-dev" in
  let module_formatted_date = CArray.of_string "juin 17 2026 23:13:30" in
  let module_date = U32.of_int 20260617 in
  let module_time = U32.of_int 231330 in

  let cob_procedure_params = CArray.create ~default:(CobField.null ()) CComp 1 in

  let pmodule = CPtr.create CComp in
  let pglobal = CPtr.create CComp in
  let _res = cob_module_global_enter pmodule pglobal S32.zero S32.zero in
  let module_ = CPtr.get pmodule in
  let _global = CPtr.get pglobal in

  CobModule.set_cob_procedure_params module_ (CArray.to_ptr cob_procedure_params);

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

  let d_0 = CobDecimal.create () in
  let d_1 = CobDecimal.create () in
  let pd_0 = CPtr.create ~default:d_0 (CComp) in
  let pd_1 = CPtr.create ~default:d_1 (CComp) in
  cob_decimal_alloc [| pd_0; pd_1 |];

  CobModule.set_module_active module_
    (U32.inc (CobModule.get_module_active module_));

  let a_1 = CobFieldAttr.create
              ~type_:(CobFieldType.(to_u16 (enc COB_TYPE_NUMERIC_DISPLAY)))
              ~digits:(U16.of_int 3) () in
  let a_2 = CobFieldAttr.create
              ~type_:(CobFieldType.(to_u16 (enc COB_TYPE_ALPHANUMERIC)))
              ~flags:(CobFieldFlag.(to_u16 (enc COB_FLAG_CONSTANT))) () in
  let a_3 = CobFieldAttr.create
              ~type_:(CobFieldType.(to_u16 (enc COB_TYPE_GROUP))) () in
  let a_4 = CobFieldAttr.create
              ~type_:(CobFieldType.(to_u16 (enc COB_TYPE_ALPHANUMERIC))) () in

  let b_c = CArray.create Char ~default:' ' 24 in
  CArray.set_string b_c 0 "var1 = ";
  CArray.set_string b_c 8 "var2 = ";
  CArray.set_string b_c 16 "var3 = ";
  let b_c' = CPtr.cast UInt8 (CArray.to_ptr b_c) in
  let c_1 = CobField.create ~size:(U64.of_int 7) ~attr:a_2
              ~data:b_c' () in
  let c_2 = CobField.create ~size:(U64.of_int 7) ~attr:a_2
              ~data:(CPtr.add b_c' 8) () in
  let c_3 = CobField.create ~size:(U64.of_int 7) ~attr:a_2
              ~data:(CPtr.add b_c' 16) () in

  let b_w = CArray.create Char ~default:' ' 24 in
  CArray.set_string b_w 0 "000000";
  CArray.set_string b_w 8 "      ";
  CArray.set_string b_w 16 "000";
  let b_w' = CPtr.cast UInt8 (CArray.to_ptr b_w) in
  let f_17 = CobField.create ~size:(U64.of_int 6) ~attr:a_3
               ~data:b_w' () in
  let f_18 = CobField.create ~size:(U64.of_int 3) ~attr:a_1
               ~data:b_w' () in
  let f_19 = CobField.create ~size:(U64.of_int 3) ~attr:a_1
               ~data:(CPtr.add b_w' 3) () in
  let f_20 = CobField.create ~size:(U64.of_int 6) ~attr:a_4
               ~data:(CPtr.add b_w' 8) () in
  let f_21 = CobField.create ~size:(U64.of_int 3) ~attr:a_1
               ~data:(CPtr.add b_w' 16) () in

  CArray.set_string b_w 0 "888";
  cob_display S32.zero S32.one [| c_1; f_17 |];
  CArray.set_string b_w 3 "999";
  cob_display S32.zero S32.one [| c_1; f_17 |];

  let b = CArray.of_string "var3 = var1.left + var1.right" in
  let f = CobField.create ~size:(U64.of_int 29) ~attr:a_2
            ~data:(CPtr.cast UInt8 (CArray.to_ptr b)) () in
  cob_display S32.zero S32.one [| f |];

  cob_decimal_set_field d_0 f_18;
  cob_decimal_set_field d_1 f_19;
  cob_decimal_add d_0 d_1;
  let _res = cob_decimal_get_field d_0 f_21 S32.zero in

  let b = CArray.of_string "memcpy var1 to var2" in
  let f = CobField.create ~size:(U64.of_int 19) ~attr:a_2
            ~data:(CPtr.cast UInt8 (CArray.to_ptr b)) () in
  cob_display S32.zero S32.one [| f |];

  CArray.blit b_w 0 b_w 8 6;

  cob_display S32.zero S32.one [| c_1; f_17 |];
  cob_display S32.zero S32.one [| c_2; f_20 |];
  cob_display S32.zero S32.one [| c_3; f_21 |];

  CobModule.set_module_active module_
    (U32.dec (CobModule.get_module_active module_));

  cob_module_leave module_

let () =

  let argv = CArray.create (CPtr Char) 2 in
  CArray.set argv 0 (CArray.to_ptr (CArray.of_string "main"));
  CArray.set argv 1 (CPtr.null Char);

  cob_init S32.one (CArray.to_ptr argv);
  run ();
  cob_stop_run S32.zero
