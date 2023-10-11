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

type lexloc = Cobol_common.Srcloc.lexloc
type srcloc = Cobol_common.Srcloc.srcloc

type 'a with_loc = 'a Cobol_common.Srcloc.with_loc =
  {
    payload: 'a;
    loc: srcloc [@compare fun _ _ -> 0];
  }
[@@deriving ord]

let pp_with_loc = Cobol_common.Srcloc.pp_with_loc
