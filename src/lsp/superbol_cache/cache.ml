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


open Types
open Store.TYPES

module TYPES = struct

  type cache =
    {
      cache_store: Store.TYPES.store;
      cache_touch_granularity: float;
    }

  (** Validating one entry usually means validating the entries it reused, and
      those overlap heavily: the documents of a project share most of their
      copybooks.  A session therefore remembers, for the duration of a single
      lookup, both what the resolver has already been asked and which entries
      have already been found to hold.  It also remembers which entries are
      currently being validated, which is how a cycle is caught. *)
  type session =
    {
      session_resolve: witness -> bool;
      session_entries: (key, bool) Hashtbl.t;
      session_witnesses: (witness, bool) Hashtbl.t;
      mutable session_visiting: key list;
    }

end
open TYPES

type t = cache

exception Cyclic of key list

let create ?(touch_granularity = 3600.) ~store () =
  {
    cache_store = store;
    cache_touch_granularity = touch_granularity;
  }

let disabled =
  create ~store:Store.unavailable ()

let shallow_key = Manifest.shallow_key

(* --- Lookup --- *)

let read_manifest { cache_store; _ } key =
  match cache_store.read (Layout.manifest key) with
  | Error _ | Ok None -> None
  | Ok Some contents -> Result.to_option (Manifest.decode contents)

(* Refreshing the date on every hit would make a read cost a write, which is
   what a cache on a network mount can least afford.  Lagging by at most the
   configured granularity is enough for pruning. *)
let touch_entry { cache_store; cache_touch_granularity; _ } key =
  let path = Layout.manifest key in
  match cache_store.stat path with
  | Ok Some { item_last_hit; _ }
    when cache_store.now () -. item_last_hit <= cache_touch_granularity ->
      ()
  | _ ->
      cache_store.touch path

let rec witness_holds t session witness =
  match witness with
  | Sub { key } ->
      entry_holds t session key
  | File _ | Dir _ | Env _ ->
      match Hashtbl.find_opt session.session_witnesses witness with
      | Some holds -> holds
      | None ->
          let holds = session.session_resolve witness in
          Hashtbl.replace session.session_witnesses witness holds;
          holds

and entry_holds t session key =
  match Hashtbl.find_opt session.session_entries key with
  | Some holds ->
      holds
  | None ->
      if List.mem key session.session_visiting then
        raise (Cyclic (List.rev (key :: session.session_visiting)));
      session.session_visiting <- key :: session.session_visiting;
      let holds = match read_manifest t key with
        | None -> false
        | Some { manifest_witnesses; _ } ->
            List.for_all (witness_holds t session) manifest_witnesses
      in
      session.session_visiting <- List.tl session.session_visiting;
      Hashtbl.replace session.session_entries key holds;
      if holds then touch_entry t key;
      holds

let read_value ({ cache_store; _ } as t) ~key ~read_item manifest_object =
  match cache_store.read (Layout.object_ manifest_object) with
  | Error msg ->
      Error (Storage msg)
  | Ok None ->
      Ok None                        (* reclaimed between manifest and value *)
  | Ok (Some item) ->
      match Result.bind (Caching.decode item) read_item with
      | Error _ -> Ok None           (* another producer's, or damaged *)
      | Ok value -> touch_entry t key; Ok (Some value)

let lookup t ~key ~resolve ~read_item =
  let session =
    { session_resolve = resolve;
      session_entries = Hashtbl.create 16;
      session_witnesses = Hashtbl.create 64;
      session_visiting = [] }
  in
  match read_manifest t key with
  | None ->
      Ok None
  | Some { manifest_witnesses; manifest_object; _ } ->
      match List.for_all (witness_holds t session) manifest_witnesses with
      | true -> read_value t ~key ~read_item manifest_object
      | false -> Ok None
      | exception Cyclic keys -> Error (Cycle keys)

(* --- Storing --- *)

(* Values are shared by every entry that holds the same bytes, and telling
   whether any other entry names one would mean reading all of them.  A value
   that has just stopped being named by the entry that held it is reclaimed
   anyway: in practice, values are seldom shared beyond the versions of one
   subject, which the callers below take care of; and an entry left pointing at
   nothing is merely a miss, after which storing its value again repairs it. *)
let remove_value { cache_store; _ } ~unless_one_of digest =
  if not (List.mem digest unless_one_of) then
    cache_store.remove (Layout.object_ digest)

let store ({ cache_store; _ } as t) ~key ~label ~witnesses ~write_item value =
  match write_item value with
  | Error err ->
      Error (Unreadable err)
  | Ok payload ->
      (* The value is published before the manifest that names it, so that an
         instance dying in between leaves behind an unreferenced value -- which
         the pruner reclaims -- rather than an entry pointing at nothing.

         The value itself is never replaced: it is named by its own digest, so
         anything already there is the very same bytes.  The manifest is, and
         has to be: when a dependency changes, the work is redone and lands
         under the same key -- the key only covers what was known beforehand --
         so refusing to replace it would leave the stale witnesses in place and
         the entry would never validate again. *)
      let item = Caching.encode payload in
      let manifest_object = Digest.string item in
      let manifest =
        { manifest_label = label;
          manifest_witnesses = witnesses;
          manifest_object }
      in
      let replaced = read_manifest t key in
      let published =
        Result.bind
          (cache_store.publish ~path:(Layout.object_ manifest_object)
             ~contents:item ~replace:false)
          begin fun () ->
            cache_store.publish ~path:(Layout.manifest key)
              ~contents:(Manifest.encode manifest) ~replace:true
          end
      in
      (* The value of the manifest just replaced is named by nothing now, unless
         it was the very same. *)
      begin match published, replaced with
        | Ok (), Some { manifest_object = previous; _ } ->
            remove_value t previous ~unless_one_of:[manifest_object]
        | _ ->
            ()
      end;
      Result.map_error (fun msg -> Storage msg) published

(* --- Superseding --- *)

let read_slot { cache_store; _ } slot =
  match cache_store.read (Layout.slot slot) with
  | Error _ | Ok None -> []
  | Ok Some contents ->
      List.filter Layout.is_key (String.split_on_char '\n' contents)

let supersede ({ cache_store; _ } as t) ~slot keys =
  let keys = List.sort_uniq String.compare keys in
  let previous = read_slot t slot in
  if previous = keys then Ok ()          (* the usual case: nothing to write *)
  else
    match
      cache_store.publish ~path:(Layout.slot slot)
        ~contents:(String.concat "\n" keys) ~replace:true
    with
    | Error msg ->
        Error (Storage msg)
    | Ok () ->
        let held =
          List.filter_map (fun key -> Option.map (fun m -> m.manifest_object)
                              (read_manifest t key)) keys
        in
        List.iter begin fun key ->
          if not (List.mem key keys) then
            match read_manifest t key with
            | None ->
                ()
            | Some { manifest_object; _ } ->
                (* The manifest goes first, so that no entry is ever seen
                   pointing at a value that is gone. *)
                cache_store.remove (Layout.manifest key);
                remove_value t manifest_object ~unless_one_of:held
        end previous;
        Ok ()

(* --- Monitoring --- *)

let size_of store path =
  match store.stat path with
  | Ok (Some { item_size; _ }) -> item_size
  | Ok None | Error _ -> 0L

let entries { cache_store; _ } =
  List.filter_map begin fun (key, path) ->
    match cache_store.read path with
    | Error _ | Ok None -> None
    | Ok (Some contents) ->
        match Manifest.decode contents with
        | Error _ -> None
        | Ok { manifest_label; manifest_witnesses; manifest_object } ->
            let info_last_hit = match cache_store.stat path with
              | Ok (Some { item_last_hit; _ }) -> item_last_hit
              | Ok None | Error _ -> 0.
            in
            Some { info_key = key;
                   info_label = manifest_label;
                   info_witnesses = manifest_witnesses;
                   info_disk_usage =
                     Int64.add (size_of cache_store path)
                       (size_of cache_store (Layout.object_ manifest_object));
                   info_last_hit }
  end (Store.entries_under cache_store Layout.manifests) |>
  Result.ok

let total_size store dir =
  List.fold_left (fun total (_, path) -> Int64.add total (size_of store path))
    0L (Store.entries_under store dir)

let stats ({ cache_store; _ } as t) =
  Result.map begin fun entries ->
    { stats_entries = List.length entries;
      stats_objects =
        List.length (Store.entries_under cache_store Layout.objects);
      stats_disk_usage =
        Int64.add (total_size cache_store Layout.manifests)
          (total_size cache_store Layout.objects);
      stats_format_version = Layout.version_dir;
      stats_producer = Producer.tag }
  end (entries t)

let prune { cache_store; _ } ~policy =
  Prune.run ~store:cache_store ~policy
