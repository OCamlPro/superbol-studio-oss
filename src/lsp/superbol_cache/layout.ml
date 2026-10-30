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


(** Where things sit under the cache root.

    Every path starts with the format version, so that instances built against
    another layout occupy a disjoint subtree of the same root and neither reads
    nor reclaims what it does not understand.

    Manifests and values are spread over a level of two-hexadecimal-digit
    directories.  A single flat directory of several thousand entries is slow to
    list on the network mounts this cache is expected to live on, and slow to
    look up in on file-systems without directory indexing. *)

let version_dir = Producer.format_version
let manifests = version_dir ^ "/manifests"
let objects = version_dir ^ "/objects"
let lock = version_dir ^ "/lock"

(** Where a store stages an item before publishing it.  Kept out of the fanout
    directories so that a half-written item is never mistaken for an entry, and
    within the format's own subtree so that it shares its file-system. *)
let tmp = version_dir ^ "/tmp"

(** Two hexadecimal digits are enough to keep directories small at the scale a
    single machine reaches, and cheap to compute. *)
let fanout hex =
  if String.length hex >= 2 then String.sub hex 0 2 else "00"

let manifest key =
  Printf.sprintf "%s/%s/%s" manifests (fanout key) key

let object_ digest =
  let hex = Digest.to_hex digest in
  Printf.sprintf "%s/%s/%s" objects (fanout hex) hex

(** [is_version_dir name] holds of the top-level entries this library owns, so
    that the pruner can tell a sibling format's subtree from unrelated files a
    user may have left in the cache root. *)
let is_version_dir name =
  String.length name >= 2 && name.[0] = 'v' &&
  String.for_all (function '0'..'9' -> true | _ -> false)
    (String.sub name 1 (String.length name - 1))
