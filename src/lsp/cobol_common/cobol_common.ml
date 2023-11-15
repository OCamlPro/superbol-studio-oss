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

module Basics = Basics
module Srcloc = Srcloc
module Copybook = Copybook
module Diagnostics = Diagnostics
module Visitor = Visitor
module Behaviors = Behaviors
module Tokenizing = Tokenizing
module Symbolic = Symbolic           (* for now; may be moved elsewhere later *)

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

module Types = struct
  include Diagnostics.TYPES
  include Srcloc.TYPES
end
include Types

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

(* --- *)

(* let tmp_files = ref [] *)
(* let remove_temporary_files = ref true *)

(* let add_temporary_file file = tmp_files := file :: !tmp_files *)
(* let keep_temporary_files () = remove_temporary_files := false *)

(* ;; *)

(* at_exit begin fun () -> *)
(*   if !remove_temporary_files then *)
(*     List.iter (fun file -> Sys.remove file) !tmp_files *)
(* end *)
