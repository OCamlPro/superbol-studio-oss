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

open Cobol_ptree                                            (* for qualname *)
open Cir_types
open Types

open Cobol_common.Srcloc.INFIX

(* --- *)

let lookup_named_field_in_map (qn: qualname with_loc) map =
  try Ok (FIELDS_MAP.find ~&qn map) with
  | Not_found ->
      Error.one @@ Undefined { stuff = Data_reference ~&qn; loc = ~@qn }
  | Cobol_unit.Resolver_map.Ambiguous candidates ->
      Error.one @@ Ambiguous { stuff = Data_reference ~&qn; loc = ~@qn;
                               candidates = Lazy.force candidates }

let lookup_named_field (qn: qualname with_loc) env =
  lookup_named_field_in_map qn env.named_fields
