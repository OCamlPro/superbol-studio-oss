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
