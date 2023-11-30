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

(** [unknown kind given_qualname] ... *)
val unknown
  : string
  -> Cobol_ptree.qualname
  -> unit

(** [ambiguous kind given_qualname ~matching_qualnames] ... *)
val ambiguous
  : string
  -> Cobol_ptree.qualname
  -> matching_qualnames: Cobol_ptree.qualname Cobol_common.Basics.NEL.t
  -> unit
