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

open Types

open Syntax

(* --- *)

let of_cobol_unit ~vm (unit: Cobol_unit.Types.t) =
  let source_file =
    let start_pos, _ =
      Cobol_common.Srcloc.forget_preproc ~@unit ~traverse_copies:false
        ~favor_direction:`Left ~traverse_replaces:false
    in
    start_pos.Lexing.pos_fname
  in
  let module_memory =
    vm.create_module ~name:~&(~&unit.unit_name) ~source_file
  in
  let* module_fields = Data_builder.create_fields_map ~vm ~&unit.unit_data in
  let env =
    Env.TYPES.{
      named_fields = module_fields.map;
      const_fields = CONST_TABLE.create 42;
      vm;
    }
  in
  let* proc = Proc_builder.translate_procedure env ~&unit.unit_procedure in
  Ok {
    module_memory;
    module_unit = unit;
    module_fields;
    module_proc = proc;
    module_initialized = false;
  }

let init_field ~vm status f =
  Error.acc_errors (vm.init_field ~vm f) status

let init_fields ~vm fields =
  List.fold_left (init_field ~vm) (Ok ()) fields

let init ~vm (m: _ module_handle) =
  let init_working_status =
    if m.module_initialized
    then Ok ()
    else init_fields ~vm m.module_fields.working_storage
  and init_local_status =
    init_fields ~vm m.module_fields.local_storage
  in
  let* () = Error.acc_errors init_working_status init_local_status in
  m.module_initialized <- true;
  Ok ()
