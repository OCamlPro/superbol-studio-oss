(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** [create ~cache_max_age ~cache_max_size ()] gathers the cache settings.

    [cache_max_age] (in seconds) and [cache_max_size] (in bytes) bound what a
    project's cache keeps; with neither, it grows without limit. *)
val pruning_policy
  : ?cache_max_age:float
  -> ?cache_max_size:int64
  -> unit
  -> Types.prune_policy
