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

open Cobol_ptree.Types

(* Note: we can share the same source overlay manager across several parsers as
   long as we don't parse localized tokens using multiple instances of the
   parser and in parallel.  If that were to change, the manager would need to be
   passed as parameter to the grammar. *)
module Overlay_manager =
  Cobol_preproc.Src_overlay.New_manager (struct
    let name = __MODULE__
  end) ()

let relation_condition ~neg (binrel: binary_relation) = function
  | None ->
      Cobol_ptree.Terms_helpers.neg_condition ~neg @@ Relation binrel
  | Some (LOr, flatop) ->
      Abbrev (neg, binrel, LOr, flatop)
  | Some (LAnd, flatop) ->
      Abbrev (neg, binrel, LAnd, flatop)
