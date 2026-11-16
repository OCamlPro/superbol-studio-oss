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

type options =
  {
    binary_size: binary_size;
    binary_byteorder: binary_byteorder;
    target_byte_order: byte_order;  (** byte order of the target machine *)
  }

and binary_size = Cobol_config.Types.binary_size
and binary_byteorder = Cobol_config.Types.binary_byteorder
and byte_order = Cobol_data.Types.byte_order
