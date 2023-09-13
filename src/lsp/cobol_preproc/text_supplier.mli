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

(** {2 Feeding the parser(s)} *)

type 'b supplier = unit -> 'b * Lexing.position * Lexing.position

val pptoks_of_text_supplier
  : (module Src_overlay.MANAGER)
  -> Text.t
  -> Preproc_tokens.token supplier

val supply_text_if_compiler_directive
  : (module Src_overlay.MANAGER)
  -> Text.t
  -> (Preproc_tokens.token supplier, [> `NotCDir]) result
