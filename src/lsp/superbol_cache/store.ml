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


(** Storage the cache is built upon.

    Like {!Cobol_common.Platform}, this is a record of operations rather than a
    set of direct system calls, so that the library itself needs no dependency
    on {!Unix}: [cobol_lsp] links against a platform that performs no I/O at
    all, and the concrete implementation is injected at startup by
    [superbol_platform].  It also lets the test suite run the whole cache
    against {!in_memory}, with no file-system involved.

    Every path handed to a store is relative to the cache root.

    {2 What an implementation must guarantee}

    The cache directory may sit on a network mount, and may be written to by
    several users at once.  An implementation is therefore required to:

    - make {!TYPES.publish} {e atomic and non-destructive}: a reader either sees
      nothing at the given path or sees the complete contents, never a prefix;
      and, unless asked to replace it, publishing over an existing path leaves
      that path alone and reports success.  Publishing is expected to stage the
      item under {!Layout.tmp}, which shares the target's file-system, followed
      by a hard link --- [link] fails with [EEXIST] exactly when someone else
      has already published the same item, which is the wanted behaviour and is
      reliable over NFS.  [rename] is only fit for replacing a target, or as a
      fallback where hard links are unavailable and the target was just seen to
      be absent: it silently overwrites, and some SMB shares refuse to replace
      with it at all.

    - report failure rather than raise.  A cache is optional by nature, so every
      operation that cannot be carried out is reported as an [Error], which the
      cache turns into a miss.

    - treat {!TYPES.touch} and {!TYPES.remove} as best-effort.  Refreshing the
      hit date of an entry owned by another user may be refused, and removing an
      item another process holds open fails on Windows; neither is worth
      reporting.

    Conversely, an implementation is {e not} required to make {!TYPES.stat}
    accurate: attribute caching makes sizes and dates stale over NFS.  The cache
    never relies on them for correctness, only for pruning and monitoring. *)

module TYPES = struct

  (** What {!TYPES.store.stat} reports about an item. *)
  type item_info =
    {
      item_size: int64;
      item_last_hit: float;       (** seconds since the epoch *)
    }

  type store =
    {
      read: string -> (string option, string) result;
      (** [read path] is [Ok None] if no item exists at [path]. *)

      publish: path:string -> contents:string -> replace:bool ->
        (unit, string) result;
      (** [publish ~path ~contents ~replace] makes [contents] appear at [path],
          creating intermediate directories.  A reader sees either nothing there
          or the whole of [contents], never a prefix.

          When [replace] is false, an item already at [path] is left alone and
          the call still succeeds: that is what two instances computing the same
          value should do to each other.  When it is true, [contents] takes the
          place of whatever was there, last writer winning. *)

      list: string -> (string list, string) result;
      (** [list dir] is the names of the items directly under [dir], without
          their directory part.  An absent directory lists as [Ok []]. *)

      stat: string -> (item_info option, string) result;
      (** [stat path] is [Ok None] unless [path] holds an item, so that the
          pruner can tell one from a directory to descend into. *)

      touch: string -> unit;
      (** [touch path] records that [path] has just been hit.  Best-effort. *)

      remove: string -> unit;
      (** [remove path] reclaims [path].  Best-effort. *)

      with_lock: 'a. string -> f:(unit -> 'a) -> ('a option, string) result;
      (** [with_lock path ~f] runs [f] with [path] held exclusively, and is
          [Ok None] if the lock is held elsewhere.  Implementations are expected
          to create the lock with [O_CREAT | O_EXCL] rather than to rely on
          [flock], whose semantics over NFS and SMB cannot be counted on, and to
          break a lock left behind by a process that died. *)

      now: unit -> float;
      (** Seconds since the epoch. *)
    }

end

open TYPES

(** A store on which nothing can be stored.  Every lookup against it misses and
    every write is dropped, which is precisely how the cache behaves when
    caching is turned off. *)
let unavailable =
  let unavailable _ = Error "no storage available" in
  {
    read = (fun _ -> Ok None);
    publish = (fun ~path:_ ~contents:_ ~replace:_ -> unavailable ());
    list = (fun _ -> Ok []);
    stat = (fun _ -> Ok None);
    touch = ignore;
    remove = ignore;
    with_lock = (fun _ ~f:_ -> Ok None);
    now = (fun () -> 0.);
  }

(** [entries_under store dir] lists the items held under the two-level fanout
    at [dir], as pairs of an item's name and its path.  Both the manifests and
    the values are laid out that way, and both the monitoring API and the pruner
    need to walk them. *)
let entries_under store dir =
  match store.list dir with
  | Error _ -> []
  | Ok fanouts ->
      List.concat_map begin fun fanout ->
        let dir = dir ^ "/" ^ fanout in
        match store.list dir with
        | Error _ -> []
        | Ok names -> List.map (fun name -> name, dir ^ "/" ^ name) names
      end fanouts

(** [in_memory ?now ()] is a store backed by a hash table.  Intended for
    testing: it honours the same contract as a file-system store, including the
    non-destructive behaviour of {!TYPES.publish}. *)
let in_memory ?now () =
  let items: (string, string * float) Hashtbl.t = Hashtbl.create 42 in
  let clock = match now with
    | Some now -> now
    | None -> let tick = ref 0. in fun () -> tick := !tick +. 1.; !tick
  in
  let locks = Hashtbl.create 7 in
  {
    read = begin fun path ->
      Ok (Option.map fst (Hashtbl.find_opt items path))
    end;
    publish = begin fun ~path ~contents ~replace ->
      if replace || not (Hashtbl.mem items path) then
        Hashtbl.replace items path (contents, clock ());
      Ok ()
    end;
    list = begin fun dir ->
      let dir = if dir = "" || dir.[String.length dir - 1] = '/'
        then dir else dir ^ "/" in
      Hashtbl.fold begin fun path _ acc ->
        let prefix = String.length dir in
        if String.length path > prefix && String.sub path 0 prefix = dir
        then
          let rest = String.sub path prefix (String.length path - prefix) in
          match String.index_opt rest '/' with
          | None -> rest :: acc
          | Some i -> String.sub rest 0 i :: acc
        else acc
      end items [] |>
      List.sort_uniq String.compare |>
      Result.ok
    end;
    stat = begin fun path ->
      Ok (Hashtbl.find_opt items path |>
          Option.map begin fun (contents, last_hit) ->
            { item_size = Int64.of_int (String.length contents);
              item_last_hit = last_hit }
          end)
    end;
    touch = begin fun path ->
      match Hashtbl.find_opt items path with
      | Some (contents, _) -> Hashtbl.replace items path (contents, clock ())
      | None -> ()
    end;
    remove = (fun path -> Hashtbl.remove items path);
    with_lock = begin fun path ~f ->
      if Hashtbl.mem locks path then Ok None
      else begin
        Hashtbl.replace locks path ();
        Fun.protect (fun () -> Ok (Some (f ())))
          ~finally:(fun () -> Hashtbl.remove locks path)
      end
    end;
    now = clock;
  }
