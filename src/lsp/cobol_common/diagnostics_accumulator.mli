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

module type S = Diagnostics_accumulator_sig.S

module MAKE
    (Set: sig
       type t
       val none: t
       val union: t -> t -> t
       val translate: t -> Diagnostics.diagnostics                (* temporary *)
     end)
  : S with type t := Set.t
