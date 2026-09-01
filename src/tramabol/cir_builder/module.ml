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

open Cir_types
open Types

open Syntax

(* --- *)

let unit_source_file unit =
  let start_pos, _ =
    Cobol_common.Srcloc.forget_preproc ~@unit ~traverse_copies:false
      ~favor_direction:`Left ~traverse_replaces:false
  in
  start_pos.Lexing.pos_fname

let of_cobol_unit ~builder (unit: Cobol_unit.Types.t) =
  let module_memory =
    builder.create_module_memory ~name:~&(~&unit.unit_name)
      ~source_file:(unit_source_file unit)
  in
  let* module_fields = Data_builder.create_fields_map ~builder ~&unit.unit_data in
  let env =
    Env.TYPES.{
      named_fields = module_fields.map;
      const_fields = CONST_TABLE.create 42;
      builder;
    }
  in
  let* proc = Proc_builder.translate_procedure env ~&unit.unit_procedure in
  Ok {
    module_memory;
    module_unit = unit;
    module_fields;
    module_proc = proc;
    (* module_initialized = false; *)
  }
