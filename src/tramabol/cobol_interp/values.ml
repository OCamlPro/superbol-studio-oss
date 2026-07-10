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

let manager =
  Cobol_ir.Types.{
    create_record_data = Record.create;
    create_mutable_field = Field.in_record_memory;
    create_field_from_literal = Field.from_literal;

    create_module = Module.create;
    enter_module = Module.enter;
    leave_module = Module.leave;

    init_field = Field.init;
    field_as_int = Field.as_int;

    display_fields = Termio.display_fields;
  }
