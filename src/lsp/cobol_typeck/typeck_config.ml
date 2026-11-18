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
    dialect: Cobol_dialect.Types.id;
    alphanumeric_comp_x_over_8_chars_rejected_by: Cobol_dialect.Types.id option;
    (** if set, warn about COMP-X and COMP-N items with an alphanumeric PICTURE
        of more than 8 characters, which this dialect rejects *)
    free_redefines_position: unit Cobol_dialect.Types.binding;
  }

and binary_size = Cobol_config.Types.binary_size
and binary_byteorder = Cobol_config.Types.binary_byteorder
and byte_order = Cobol_data.Types.byte_order

(** Options derived from the given dialect configuration, for a target machine
    with the given byte order. *)
let from_cobol_config ~target_byte_order (config: Cobol_config.t) =
  {
    binary_size = config.binary_size;
    binary_byteorder = config.binary_byteorder;
    target_byte_order;
    dialect = config.origin.id;
    alphanumeric_comp_x_over_8_chars_rejected_by =
      Cobol_dialect.Origin.closest_ancestor
        (function MicroFocus _ -> true | _ -> false) config.origin;
    free_redefines_position = config.free_redefines_position;
  }
