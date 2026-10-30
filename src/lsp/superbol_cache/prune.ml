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

  (** What a pruning round needs to know about one entry. *)
  type entry =
    {
      entry_path: string;
      entry_last_hit: float;
      entry_size: int64;                       (** of the manifest itself *)
      entry_value: digest option;              (** the value it holds, if any *)
    }

end
open TYPES

(* A value is published just before the entry that names it, so one that has
   only just appeared may well belong to an entry that was not there yet when
   the manifests were listed.  Such a value is spared, and reclaimed by a later
   round.  The window is deliberately short and unrelated to how long entries
   are kept: it guards against a race, not against disuse.  It is narrowed when
   entries are kept for less than that, so that asking for everything to go does
   take everything. *)
let publication_grace = 60.

let grace ~max_age = match max_age with
  | Some max_age -> Float.min max_age publication_grace
  | None -> publication_grace

(* An item is anything [stat] describes; everything else that [list] reports is
   taken to be a directory to descend into.  The cache writes no other kind of
   entry, and is deliberately incurious about any it did not write. *)
let rec items store dir =
  match store.list dir with
  | Error _ -> []
  | Ok names ->
      List.concat_map begin fun name ->
        let path = if dir = "" then name else dir ^ "/" ^ name in
        match store.stat path with
        | Ok (Some info) -> [path, info]
        | Ok None -> items store path
        | Error _ -> []
      end names

let size_of store path =
  match store.stat path with
  | Ok (Some { item_size; _ }) -> item_size
  | Ok None | Error _ -> 0L

let total_size items =
  List.fold_left (fun total (_, size) -> Int64.add total size) 0L items

(* --- Reading the entries out --- *)

(** Every entry currently held, and whether all of them could be read.  Nothing
    may be swept unless they could: a value is reclaimed for being named by no
    entry, and an entry that could not be read names nothing we can see. *)
let entries store =
  List.fold_left begin fun (entries, complete) (_key, entry_path) ->
    match store.stat entry_path, store.read entry_path with
    | Ok (Some { item_size; item_last_hit }), Ok (Some contents) ->
        let entry_value = match Manifest.decode contents with
          | Ok { manifest_object; _ } -> Some manifest_object
          | Error _ -> None
        in
        { entry_path; entry_last_hit = item_last_hit;
          entry_size = item_size; entry_value } :: entries,
        complete && entry_value <> None
    | _, Ok None | Ok None, _ ->
        entries, complete                   (* vanished under us; nothing to keep *)
    | _ ->
        entries, false
  end ([], true) (Store.entries_under store Layout.manifests)

let values store =
  List.map (fun (hex, path) -> hex, path, size_of store path)
    (Store.entries_under store Layout.objects)

(* --- The bounds --- *)

let reclaim_aged store ~now ~max_age entries =
  match max_age with
  | None ->
      0, entries
  | Some max_age ->
      List.partition (fun { entry_last_hit; _ } ->
          now -. entry_last_hit > max_age) entries |>
      fun (aged, kept) ->
      List.iter (fun { entry_path; _ } -> store.remove entry_path) aged;
      List.length aged, kept

(** Reclaims the least recently used entries until what is left fits.

    A value may be held by several entries, so evicting one only frees its value
    once nothing else names it; the count is kept as the eviction goes, rather
    than measuring the cache again after each one. *)
let reclaim_oversized store ~max_size ~held entries =
  let references = Hashtbl.create 64 in
  let bump hex n =
    Hashtbl.replace references hex
      (n + Option.value ~default:0 (Hashtbl.find_opt references hex))
  in
  List.iter begin function
    | { entry_value = Some value; _ } -> bump (Digest.to_hex value) 1
    | { entry_value = None; _ } -> ()
  end entries;
  let value_size = Hashtbl.create 64 in
  List.iter (fun (hex, _, size) -> Hashtbl.replace value_size hex size) held;
  let total =
    ref (Int64.add
           (total_size (List.map (fun e -> e.entry_path, e.entry_size) entries))
           (total_size (List.map (fun (hex, _, size) -> hex, size) held)))
  in
  let reclaimed = ref 0 and kept = ref [] in
  List.sort (fun a b -> Float.compare a.entry_last_hit b.entry_last_hit) entries |>
  List.iter begin fun ({ entry_path; entry_size; entry_value; _ } as entry) ->
    if !total <= max_size then kept := entry :: !kept else begin
      store.remove entry_path;
      incr reclaimed;
      total := Int64.sub !total entry_size;
      match entry_value with
      | None -> ()
      | Some value ->
          let hex = Digest.to_hex value in
          bump hex (-1);
          if Option.value ~default:0 (Hashtbl.find_opt references hex) <= 0
          then begin
            store.remove (Layout.object_ value);
            total := Int64.sub !total
                (Option.value ~default:0L (Hashtbl.find_opt value_size hex))
          end
    end
  end;
  !reclaimed, !kept

(** Reclaims every value no surviving entry names. *)
let sweep_unheld_values store ~now ~grace ~entries held =
  let named =
    List.filter_map (fun { entry_value; _ } ->
        Option.map Digest.to_hex entry_value) entries
  in
  List.iter begin fun (hex, path, _) ->
    let just_published = match store.stat path with
      | Ok (Some { item_last_hit; _ }) -> now -. item_last_hit <= grace
      | Ok None | Error _ -> true
    in
    if not (just_published || List.mem hex named) then store.remove path
  end held

(* An instance that died between staging an item and publishing it leaves the
   staged copy behind. *)
let reclaim_stale_staging store ~now ~grace =
  List.iter begin fun (path, { item_last_hit; _ }) ->
    if now -. item_last_hit > grace then store.remove path
  end (items store Layout.tmp)

(* A neighbouring format's subtree is left entirely alone until all of it has
   gone stale: an instance built against it may well be running right now, and
   its layout is none of our business.  With no age bound there is no telling
   when that is, so it is left alone altogether. *)
let reclaim_stale_formats store ~now ~max_age =
  match max_age, store.list "" with
  | None, _ | _, Error _ -> ()
  | Some max_age, Ok names ->
      List.iter begin fun name ->
        if Layout.is_version_dir name && name <> Layout.version_dir then
          let contents = items store name in
          let all_stale =
            contents <> [] &&
            List.for_all
              (fun (_, { item_last_hit; _ }) -> now -. item_last_hit > max_age)
              contents
          in
          if all_stale then
            List.iter (fun (path, _) -> store.remove path) contents
      end names

(* --- *)

let run ~store ~policy =
  match policy with
  | Never ->
      Ok 0
  | Bounded { max_age; max_size } ->
      let grace = grace ~max_age in
      let prune () =
        let now = store.now () in
        let entries, complete = entries store in
        let aged, entries = reclaim_aged store ~now ~max_age entries in
        let oversized, entries = match max_size with
          | Some max_size when complete ->
              reclaim_oversized store ~max_size ~held:(values store) entries
          | Some _ | None ->
              0, entries
        in
        if complete then
          sweep_unheld_values store ~now ~grace ~entries (values store);
        reclaim_stale_staging store ~now ~grace;
        reclaim_stale_formats store ~now ~max_age;
        aged + oversized
      in
      match store.with_lock Layout.lock ~f:prune with
      | Ok (Some reclaimed) -> Ok reclaimed
      | Ok None -> Ok 0                    (* someone else is already pruning *)
      | Error msg -> Error (Storage msg)
