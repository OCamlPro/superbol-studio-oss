(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_data.Types

module MEM = Cobol_data.Memory
module PIC = Cobol_data.Picture

let size_of ~usage =
  match usage with
  | Binary picture ->
      let len = PIC.data_size picture in
      let num_bytes =                   (* (from David's analyzer) *)
        if len < 3 then 1               (* 1-2 = 1 byte *)
        else if len < 5 then 2          (* 3-4 = 2 bytes *)
        else if len < 10 then 4         (* 5-9 = 4 bytes *)
        else if len < 20 then 8         (* 10-19 = 8 bytes *) (* 18 if signed *)
        else 16
      in
      MEM.(mult_int byte_size num_bytes)
  | Binary_char _ ->
      MEM.byte_size
  | Binary_C_long _ ->
      MEM.size_of_C_long
  | Binary_double _ ->
      MEM.(mult_int byte_size 8)
  | Binary_long _ ->
      MEM.(mult_int byte_size 4)
  | Binary_short _ ->
      MEM.(mult_int byte_size 2)
  | Bit picture ->                                     (* TODO: probably wrong *)
      MEM.(mult_int bit_size @@ PIC.data_size picture)
  | Display picture ->
      MEM.(mult_int byte_size @@ PIC.data_size picture)
  | Float_binary { width = `W32; _ } ->
      MEM.(mult_int byte_size 4)
  | Float_binary { width = `W64; _ } ->
      MEM.(mult_int byte_size 8)
  | Float_binary { width = `W128; _ } ->
      MEM.(mult_int byte_size 16)
  | Float_decimal { width = `W16; _ } ->
      MEM.(mult_int byte_size 8)
  | Float_decimal { width = `W34; _ } ->
      MEM.(mult_int byte_size 16)
  | Float_extended ->
      MEM.size_of_C_long_double
  | Float_long ->
      MEM.size_of_C_double
  | Float_short ->
      MEM.size_of_C_float
  | Function_pointer _ ->
      MEM.size_of_pointer
  | Procedure_pointer ->
      MEM.size_of_pointer
  | Index ->
      MEM.size_of_index
  | National picture ->
      MEM.(mult_int byte_size @@ PIC.data_size picture * 4)
  | Object_reference _ ->
      MEM.size_of_pointer
  | Packed_decimal picture ->
      MEM.(mult_int byte_size @@ PIC.data_size picture / 2 + 1)
  | Pointer _ ->
      MEM.size_of_pointer
  | Program_pointer _ ->
      MEM.size_of_pointer
