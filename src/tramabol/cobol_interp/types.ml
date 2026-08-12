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
open Cobol_common.Srcloc.TYPES

type cob_field = CobField.t cptr

type cob_field_handle = cob_field Cobol_ir.Types.field
type cob_field_mutable = cob_field Cobol_ir.Types.mutable_field
type cob_field_immutable = cob_field Cobol_ir.Types.immutable_field

type cob_record_handle = cob_record_data Cobol_ir.Types.record_handle
and cob_record_data =
  {
    record_data_ptr: U8.t cptr;
    record_data_size: int;                                             (* > 0 *)
  }

module FIELDS_MAP = Cobol_ir.Types.FIELDS_MAP
type fields_map = cob_field Cobol_ir.Types.fields_map

type module_handle = (cob_field, cob_module_memory) Cobol_ir.Types.module_handle
and cob_module_memory =
  {
    module_ptr: CobModule.t cptr;
    module_globals: CobGlobal.t cptr;
  }

type Cobol_ir.Types.unsupported_stuff +=
  | Literal of Cobol_ptree.literal
  | Field_usage

type Cobol_ir.Types.error +=
  | Invalid_compilation_group of
      {
        reason: [ `empty_group | `non_singleton_group ];
      }
  | Ezlibcob_error of Ezlibcob.V1.error
