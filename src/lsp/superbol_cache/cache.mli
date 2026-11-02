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


(** The dependency-aware cache.

    {2 How an entry is looked up}

    An entry is addressed by a {e shallow key}, built with {!shallow_key} from
    what the caller knows before doing any work.  The shallow key locates a
    manifest, which lists the witnesses that were observed {e while} the work was
    done.  A hit requires the manifest to be there {e and} every one of its
    witnesses to still hold; anything else is a miss.

    This is what gives the library its defining property without storing a
    graph: since a witness may be the key of another entry, a miss on a
    dependency --- however deep --- is a miss on everything that reused it.

    {2 Who performs I/O}

    The cache reads and writes only under its own root, through the {!Store}
    record it was created with.  Deciding whether a witness still holds is the
    caller's job, via the [resolve] argument of {!lookup}: the LSP answers from
    the documents it has open, which may differ from what is on disk, while the
    CLI answers from the file-system.  This is what makes a hit recorded against
    an editor buffer and a hit recorded against a file name interchangeable, as
    long as the contents agree.

    Use {!Witness} to build witnesses, rather than the constructors in
    {!Types}: it applies the path canonicalisation that two spellings of one
    path must agree on.

    {2 Failure}

    A cache is optional.  Every operation here reports a miss rather than a
    failure when the underlying storage misbehaves, and the [Error] cases that
    remain still only mean "carry on as though the cache were empty". *)

type t

(** [create ?touch_granularity ~store ()] is a handle on the cache held in
    [store].

    A hit refreshes the date the entry was last used, which pruning relies on.
    Doing so on every hit would turn each read into a write --- ruinous on a
    shared network mount --- so the date is only refreshed once it is
    [touch_granularity] seconds stale.  That bounds how far the recorded date
    lags behind reality, so keep it well under any pruning delay. *)
val create: ?touch_granularity:float -> store:Store.TYPES.store -> unit -> t

(** A cache that misses on everything and stores nothing, for when caching is
    turned off. *)
val disabled: t

(** [shallow_key ~label ~inputs] is the key to look an entry up by; see
    {!Manifest.shallow_key}.  [label] is carried through to the monitoring API
    and is otherwise opaque. *)
val shallow_key: label:string -> inputs:Types.digest list -> Types.key

(** {2 Looking up and storing} *)

(** [lookup t ~key ~resolve ~read_item] is [Ok (Some value)] when [key] has a
    manifest whose every witness [resolve] accepts, and whose value [read_item]
    decodes; [Ok None] on a miss.

    [resolve] is never asked about a {!Types.Sub} witness: those are followed
    within the cache.  It may be asked about the same witness more than once
    across a single call only if it is reached through unrelated entries; the
    answers are memoised for the duration of the call, so an expensive resolver
    need not memoise on its own. *)
val lookup
  : t
  -> key: Types.key
  -> resolve: (Types.witness -> bool)
  -> read_item: (string -> ('a, Types.read_error) result)
  -> ('a option, Types.error) result

(** [store t ~key ~label ~witnesses ~write_item value] records [value] under
    [key], to be returned by a later {!lookup} for as long as every one of
    [witnesses] keeps holding.

    Storing over an entry that is already there replaces it, since the work is
    redone once a dependency has changed and lands under the same key; the value
    it held is then reclaimed, unless it is the very same. *)
val store
  : t
  -> key: Types.key
  -> label: string
  -> witnesses: Types.witness list
  -> write_item: ('a -> (string, Types.read_error) result)
  -> 'a
  -> (unit, Types.error) result

(** {2 Superseding}

    Keys are computed from what is known beforehand, which for a document
    includes its contents: every version of it lands under a key of its own, and
    nothing about one version tells the cache that another is now obsolete.
    Left alone, those versions accumulate until pruning reclaims them, which
    with policy [Never] is never.

    A {e slot} is the caller's way of saying so: it names whatever the keys of
    the entries standing for one subject (say, a document and its
    configuration, regardless of contents) were last recorded to be.  Slots are
    addressed like entries, by a {!shallow_key} whose inputs leave out what
    distinguishes one version from the next.

    Note that the value of an entry that is replaced or superseded is reclaimed
    right away, without checking whether some unrelated entry holds the very
    same bytes: doing so would mean reading every entry.  Should one do, it
    becomes a miss, and storing its value again repairs it. *)

(** [supersede t ~slot keys] records [keys] as the entries currently standing
    for [slot], and removes those it recorded previously that are not among
    [keys], along with their values unless one of [keys] holds them.

    Nothing is written when [keys] are already what [slot] records.  Should two
    instances supersede the same slot concurrently, the last one wins, and at
    worst an entry is either left behind or reclaimed early: neither affects
    what a later {!lookup} returns, beyond turning a hit into a miss. *)
val supersede
  : t
  -> slot: Types.key
  -> Types.key list
  -> (unit, Types.error) result

(** {2 Monitoring and pruning} *)

(** [entries t] describes every entry held, including those written by other
    producers sharing the same on-disk format. *)
val entries: t -> (Types.entry_info list, Types.error) result

(** [stats t] aggregates {!entries} and adds the identities [t] runs under. *)
val stats: t -> (Types.stats, Types.error) result

(** [prune t ~policy] reclaims what [policy] allows, and is the number of
    entries removed; see {!Prune.run}.

    Pruning is the one operation that takes a lock, and is a no-op --- reported
    as [Ok 0] --- when another instance is already at it. *)
val prune: t -> policy:Types.prune_policy -> (int, Types.error) result
