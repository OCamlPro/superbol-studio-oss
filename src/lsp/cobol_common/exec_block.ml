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

module TYPES = struct
  type exec_block = ..
  type diagnostic = ..

  type registered_exec_block =
    {
      name: string;
      compare: exec_block -> exec_block -> int option;
      pp: exec_block -> Pretty.delayed option;
    }

  type registered_diagnostic =
    {
      diag_severity: diagnostic -> Diagnostics.severity option;
      diag_loc: diagnostic -> Srcloc.srcloc option;
      diag_pp: diagnostic -> Pretty.delayed option;
    }

end
include TYPES

type t = exec_block

let registry: (string, registered_exec_block) Hashtbl.t =
  Hashtbl.create 1

let register_exec_block_type ~name ~compare ~pp =
  Hashtbl.replace registry name { name; compare; pp }

let compare a b =
  let open struct exception Res of int end in
  try
    Hashtbl.iter begin fun _ { compare; _ } ->
      match compare a b with
      | Some r -> raise @@ Res r
      | None -> ()
    end registry;
    Stdlib.compare a b                    (* should just compare tags, right? *)
  with Res r -> r

let pp: t Pretty.printer = fun ppf exec_block ->
  try
    Hashtbl.iter begin fun _ { pp; _ } ->
      match pp exec_block with
      | Some pp -> pp ppf; raise Exit
      | None -> ()
    end registry;
    Pretty.string ppf "<unknown EXEC block>"
  with Exit -> ()

(* --- *)

let diags_registry: (string, registered_diagnostic) Hashtbl.t =
  Hashtbl.create 1

let register_diagnostic_type ~name ~severity ~loc ~pp =
  Hashtbl.replace diags_registry name { diag_severity = severity;
                                        diag_loc = loc;
                                        diag_pp = pp }

let diagnostic_severity: diagnostic -> Diagnostics.severity = fun diagnostic ->
  Hashtbl.to_seq_values diags_registry |>
  Seq.find_map (fun { diag_severity; _ } -> diag_severity diagnostic) |>
  function
  | Some res -> res
  | None ->
      Pretty.error ">> Internal warning in `%s.severity`: unknown diagnostic \
                    severity; has this type of diagnostic been registered?\
                   " __MODULE__;
      Error                                        (* note: error by default? *)

let diagnostic_loc: diagnostic -> Srcloc.srcloc option = fun diagnostic ->
  Hashtbl.to_seq_values diags_registry |>
  Seq.find_map (fun { diag_loc; _ } -> diag_loc diagnostic)

let pp_diagnostic: diagnostic Pretty.printer = fun ppf diagnostic ->
  try
    Hashtbl.iter begin fun _ { diag_pp; _ } ->
      match diag_pp diagnostic with
      | Some pp -> pp ppf; raise Exit
      | None -> ()
    end diags_registry;
    Pretty.string ppf "<unknown EXEC block diagnostic>"
  with Exit -> ()
