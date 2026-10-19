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

open Types

let register_loc_retrievers () =

  Cir_builder.Error.register_error_loc_retriever begin function
    | Invalid_compilation_group _ ->
        None
    | Ezlibcob_build_error { loc; _ } ->
        loc
    | _ ->
        None
  end
