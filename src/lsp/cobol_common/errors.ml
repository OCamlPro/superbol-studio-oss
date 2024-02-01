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

exception FatalError of string
let fatal fmt = Pretty.string_to (fun s -> raise @@ FatalError s) fmt

include Exit_status

(* Register a printer for some common exceptions. *)
let init_default_exn_printers () =
  Lazy.force @@ lazy begin
    Printexc.register_printer begin function
      | Failure m
      | Sys_error m
      | Stdlib.Arg.Bad m -> Some m
      | _ -> None
    end
  end

(* --- *)

(* TODO: move the ['a result] type related functions somewhere else *)
let join_all l =
  List.fold_left
    (fun res elt ->
       match res, elt with
       | Result.Error e, _ | _, Result.Error e -> Error e
       | Ok res, Ok e -> Ok (e::res))
    (Ok [])
    l
  |> Result.map List.rev
