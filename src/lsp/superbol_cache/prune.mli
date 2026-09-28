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


(** Reclaiming what a cache no longer needs. *)

(** [run ~store ~policy] applies [policy] to the cache held in [store], and is
    the number of entries it reclaimed.

    An age bound goes first, then a size bound reclaims the least recently used
    of whatever is left until the cache fits.  Values are shared between
    entries, so one is only freed once no entry names it any more; whatever is
    left unnamed is swept at the end.  Should an entry fail to decode, values
    are left alone for that round --- sweeping without having read every entry
    would reclaim ones that are still in use --- and so is the size bound, which
    could not be told what evicting an entry would free.

    Subtrees belonging to another on-disk format are reclaimed only once
    {e everything} in them has gone stale, so that an instance built against a
    neighbouring format sharing the root keeps working.

    Pruning takes the cache's lock, and is a no-op --- reported as [Ok 0] ---
    when another instance is already at it. *)
val run
  : store: Store.TYPES.store
  -> policy: Types.prune_policy
  -> (int, Types.error) result
