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


(** Public type definitions for the SuperBOL dependency-aware cache.

    Terminology: a {e witness} records an observation made while producing a
    cached value --- the contents of a file that was read, the listing of a
    directory that was searched, the status of an environment variable that was
    consulted, or the key of another entry that was reused.  A {e manifest}
    gathers the witnesses observed for one entry, along with the digest of the
    value that was produced.

    An entry is looked up via a {e shallow key}, computed from what is known
    {e before} the work is performed.  The shallow key locates the manifest; a
    hit additionally requires every recorded witness to still hold. *)

(** Digest of some contents; holds the raw (16-byte) output of {!Digest.string},
    {e not} its hexadecimal rendering. *)
type digest = string

(** Hexadecimal rendering of the digest that addresses a manifest. *)
type key = string

(** An observation whose invalidation entails a miss on the entry that recorded
    it.

    Paths carried by [File] and [Dir] are canonical in the sense of
    {!Superbol_cache.Canon}; they need not be meaningful on any machine other
    than the one that recorded them. *)
type witness =
  | File of
      {
        name: string;          (** canonical path of the file that was read *)
        digest: digest;        (** digest of its contents *)
      }
  | Dir of
      {
        name: string;          (** canonical path of the directory searched *)
        digest: digest;        (** digest of its sorted, canonical listing *)
      }
  | Env of
      {
        var: string;           (** name of the variable that was consulted *)
        value: string option;  (** [None] iff it was unset *)
      }
  | Sub of
      {
        key: key;              (** entry whose value was reused *)
      }

(** Everything recorded about one entry, besides its value. *)
type manifest =
  {
    manifest_label: string;         (** caller-chosen; for monitoring only *)
    manifest_witnesses: witness list;
    manifest_object: digest;        (** digest of the associated value *)
  }

(** Why a stored item could not be read back.

    Note that a [Bad_version_tag] is expected in normal operation: it is how
    entries left behind by a different producer are skipped. *)
type read_error =
  | Bad_version_tag of { got: string; expected: string; }
  | Truncated                   (** item shorter than its own header claims *)
  | Malformed of string         (** item could not be decoded *)
  | Absent                      (** no such item in the cache *)

(** Why a cache operation could not be carried out.  Every variant denotes a
    condition under which the caller should proceed as though the cache were
    empty; none of them is an error the caller needs to handle. *)
type error =
  | Unreadable of read_error
  | Storage of string           (** underlying storage reported a failure *)
  | Cycle of key list           (** [Sub] witnesses form a cycle *)

(** When entries are reclaimed.

    The bounds of [Bounded] compose: an entry goes if {e any} of them says so,
    and leaving them all unset reclaims nothing but the leftovers of instances
    that died mid-write. *)
type prune_policy =
  | Never                       (** no entry is ever removed automatically *)
  | Bounded of
      {
        max_age: float option;
        (** Reclaim entries that have not been hit for that many seconds. *)
        max_size: int64 option;
        (** Keep the whole cache --- entries and the values they hold --- under
            that many bytes, by reclaiming those least recently used. *)
      }

(** One row of the monitoring API. *)
type entry_info =
  {
    info_key: key;
    info_label: string;
    info_witnesses: witness list;
    info_disk_usage: int64;     (** manifest and value, in bytes *)
    info_last_hit: float;       (** seconds since the epoch *)
  }

(** Aggregate figures reported by the monitoring API. *)
type stats =
  {
    stats_entries: int;
    stats_objects: int;
    stats_disk_usage: int64;
    stats_format_version: string;
    stats_producer: string;
  }
