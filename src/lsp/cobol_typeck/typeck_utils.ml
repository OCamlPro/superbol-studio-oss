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
  | Binary { byte_size = Byte_size; _ } ->
      MEM.byte_size
  | Binary { byte_size = Short_size; _ } ->
      MEM.(mult_int byte_size 2)
  | Binary { byte_size = Long_size; _ } ->
      MEM.(mult_int byte_size 4)
  | Binary { byte_size = Double_size; _ } ->
      MEM.(mult_int byte_size 8)
  | Binary { byte_size = Long_double_size; _ } ->
      MEM.(mult_int byte_size 16)
  | Binary { byte_size = C_long_size; _ } ->
      MEM.size_of_C_long
  | Binary { byte_size = Custom_size n; _ } ->
      MEM.(mult_int byte_size n)
  | Bit picture ->                                     (* TODO: probably wrong *)
      MEM.(mult_int bit_size @@ PIC.data_size picture)
  | Alphanumeric { picture; _ }
  | Display_numeric { picture; sign = Display_unsigned } ->
      MEM.(mult_int byte_size @@ PIC.display_size picture)
  | Display_numeric { picture; sign = Display_signed { sign_separate; _ } } ->
      MEM.(mult_int byte_size @@ PIC.display_size picture ~sign_separate)
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
  | Packed_decimal { picture; with_sign_nibble } ->
      MEM.(mult_int byte_size @@
           if with_sign_nibble
           then PIC.data_size picture / 2 + 1
           else (PIC.data_size picture + 1) / 2)
  | Pointer _ ->
      MEM.size_of_pointer
  | Program_pointer _ ->
      MEM.size_of_pointer
