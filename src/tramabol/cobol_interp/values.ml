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

let builder: value_builder =
  Cir_builder.Types.{
    create_record_data = Record.create;
    create_mutable_field = Field.in_record_memory;
    create_field_from_literal_value = Field.from_literal_value;

    create_module_memory = Module.create;
  }

let manager: manager =
  Cir_logic.Types.{
    enter_module = Module.enter;
    leave_module = Module.leave;
    module_ws_needs_initialization = Module.ws_needs_initialization;
    module_ws_initialization_done = Module.ws_initialization_done;

    init_field = Field.init;
    field_as_int = Field.as_int;

    display_fields = Termio.display_fields;
  }
