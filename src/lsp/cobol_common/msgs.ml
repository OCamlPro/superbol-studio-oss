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

type addenda = (Pretty.delayed * Srcloc.srcloc option) list
exception LocalizedError of Pretty.delayed * Srcloc.srcloc * addenda
exception Error of Pretty.delayed

let localized_error ?(addenda = []) (loc: Srcloc.srcloc) =
  Pretty.delayed_to (fun s -> raise @@ LocalizedError (s, loc, addenda))

let error ?loc =
  Pretty.delayed_to (fun s -> match loc with
    | None -> raise @@ Error s
    | Some loc -> raise @@ LocalizedError (s, loc, []))

(* --- *)

let pp_msg ?loc ppf fmt =
  (* Source text right above message for now: *)
  Pretty.print ppf ("%a"^^fmt) (Pretty.option Srcloc.pp_srcloc) loc

(* --- *)

let pp_kuncaught k exn fmt =
  Pretty.string_to k ("Fatal error: uncaught exception %s:@\n"^^fmt) exn

(* Register some printing functions, useful upon misuse of
   `Cobol_common.catch_diagnostics` and co. *);;
Stdlib.Printexc.register_printer begin
  let uncaught exn fmt =
    pp_kuncaught Option.some (__MODULE__^exn) @@ fmt ^^
      "@\n(Dev hint: this is probably due to a missing call to \
       Cobol_common.catch_diagnostics)@."
  in
  function
  | LocalizedError (msg, loc, _) ->
      uncaught "LocalizedError" "%a" (fun ppf -> pp_msg ~loc ppf "%t") msg
  | Error msg ->
      uncaught "Error" "%t" msg
  | _ ->
      None
end
