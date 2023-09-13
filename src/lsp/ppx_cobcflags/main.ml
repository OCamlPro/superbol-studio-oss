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

open Ppxlib

let rule_flag_rq = Context_free.Rule.extension Flag_rq.extension_flag_rq
let rule_flag = Context_free.Rule.extension Flag.extension_flag
let rule_flag_on = Context_free.Rule.extension Flag.extension_flag_on

let () =
  Driver.register_transformation "ppx_cobcflags" ~rules:[rule_flag_rq; rule_flag; rule_flag_on]
