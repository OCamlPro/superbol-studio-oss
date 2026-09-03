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

(* --- *)

let localize_errors ~loc =
  Result.map_error @@ NEL.map ~f:begin fun error ->
    match error.loc with
    | None -> { error with loc = Some loc }
    | Some _ -> error
  end

(* --- *)

let loc_retriever_for_extended_type () =
  let l = ref [] in
  (fun (g: _ -> Cobol_common.Srcloc.TYPES.srcloc option) -> l := g :: !l),
  (fun e ->
     let rec aux = function
       | [] -> None
       | g :: tl -> match g e with None -> aux tl | Some _ as res -> res
     in
     aux !l)

let register_runtime_error_loc_retriever, loc =
  loc_retriever_for_extended_type ()

let register_loc_retrievers () =

  register_runtime_error_loc_retriever begin function
    | Unsupported_runtime_operation _ ->
        None
    | _ ->
        None
  end
