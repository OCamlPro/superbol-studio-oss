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

(** {2 Main API} *)

val default_run_options: run_options

(** Runs a module, giving one result per execution path; explores every path
    with non-deterministic managers. *)
val run_module
  : ?options: run_options
  -> vm:('f, 'b, 'r, 'm, 's, 'k) manager
  -> ('f, 'r, 'm) Cir_types.module_handle
  -> 's
  -> (('s * 'f, localized_runtime_errors) result, 'k) branches

(** {2 Low-level API} *)

val step
  : options: run_options
  -> vm:('f, 'b, 'r, 'm, 's, 'k) manager
  -> 'f Cir_types.procedure
  -> 's runtime_configuration
  -> ((('f, 's) step_outcome, 'k) branches, localized_runtime_errors) result
