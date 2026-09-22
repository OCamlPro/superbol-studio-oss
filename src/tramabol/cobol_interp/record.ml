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
open Types

let create record_definition : cob_record_handle =
  let record_data_size =
    Cobol_data.Memory.as_bytes
      (Cobol_data.Item.record_size record_definition)
      ~memory_config:Cobol_data.Memory.amd64_memory_config
  in
  let record_data_ptr =
    CPtr.cast UInt8 @@ CArray.to_ptr @@ CArray.create Char ~default:' '
      record_data_size
  in
  {
    record_memory = { record_data_ptr; record_data_size };
    record_definition;
  }
