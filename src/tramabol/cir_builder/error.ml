(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c)      2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

let one e = Error (NEL.one e)

let extra stuff ~locs =
  one @@ Extraneous { stuff; locs }

let missing stuff ~loc =
  one @@ Missing { stuff; loc }

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

let register_error_loc_retriever, loc =
  loc_retriever_for_extended_type ()

let register_loc_retrievers () =

  register_error_loc_retriever begin function
    | Ambiguous { loc; _ }
    | Missing { loc; _ }
    | Undefined { loc; _ }
    | Unsupported { loc; _ } ->
        Some loc
    | Extraneous { locs; _ } ->
        Cobol_common.Srcloc.concat_srclocs @@ NEL.to_list locs  (* never None *)
    | Data_error e ->
        Some (Cobol_data.Error.loc e)
    | _ ->
        None
  end
