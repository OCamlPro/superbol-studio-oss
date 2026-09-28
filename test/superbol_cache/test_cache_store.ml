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


(** Tests for the file-system store the cache actually runs on.

    {!Test_cache} covers the logic against an in-memory store; what is left to
    check here is what only a real file-system can show: that publishing is
    non-destructive, that entries survive a fresh handle on the same directory,
    that the pruner's lock is exclusive, and that a shared directory comes out
    group-writable. *)

open Superbol_cache
open Superbol_cache.Store.TYPES

let nth = ref 0

let root () =
  incr nth;
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "superbol-cache-test-%d-%d" (Unix.getpid ()) !nth)
  in
  (try Ez_file.V1.EzFile.remove_dir ~all:true root with _ -> ());
  at_exit begin fun () ->
    try Ez_file.V1.EzFile.remove_dir ~all:true root with _ -> ()
  end;
  root

let put cache ~key value =
  Cache.store cache ~key ~label:"unit" ~witnesses:[]
    ~write_item:Caching.write_item value

let get cache ~key =
  match Cache.lookup cache ~key ~resolve:(fun _ -> true)
          ~read_item:Caching.read_item with
  | Ok (Some value) -> Printf.printf "hit: %s\n" value
  | Ok None -> print_endline "miss"
  | Error _ -> print_endline "error"

let key what = Cache.shallow_key ~label:"unit" ~inputs:[Digest.string what]

(* --- *)

let%expect_test "an entry survives a fresh handle on the same directory" =
  let root = root () in
  let k = key "a" in
  let first = Cache.create ~store:(Superbol_platform.Cache_store.create ~root ()) () in
  Printf.printf "stored: %b\n" (put first ~key:k "parsed a" = Ok ());
  (* As a separate instance would see it. *)
  let second = Cache.create ~store:(Superbol_platform.Cache_store.create ~root ()) () in
  get second ~key:k;
  [%expect {|
    stored: true
    hit: parsed a |}];;

let%expect_test "publishing over an existing item leaves it alone" =
  let root = root () in
  let store = Superbol_platform.Cache_store.create ~root () in
  let path = Layout.manifest (key "a") in
  Printf.printf "first: %b\n" (store.publish ~path ~contents:"one" ~replace:false = Ok ());
  Printf.printf "second: %b\n" (store.publish ~path ~contents:"two" ~replace:false = Ok ());
  Printf.printf "kept: %s\n"
    (match store.read path with Ok (Some s) -> s | _ -> "<none>");
  (* Two instances that computed the same entry must not damage each other, so
     losing the race is a success that changes nothing. *)
  [%expect {|
    first: true
    second: true
    kept: one |}];;

let%expect_test "staging leaves nothing behind" =
  let root = root () in
  let store = Superbol_platform.Cache_store.create ~root () in
  ignore (store.publish ~path:(Layout.manifest (key "a")) ~contents:"one" ~replace:false);
  Printf.printf "staging is empty: %b\n" (store.list Layout.tmp = Ok []);
  [%expect {| staging is empty: true |}];;

let%expect_test "the pruner's lock is exclusive, and pruning is not blocking" =
  let root = root () in
  let store = Superbol_platform.Cache_store.create ~root () in
  let held = store.with_lock Layout.lock ~f:begin fun () ->
    (* A second instance must find the lock taken and give up rather than wait:
       pruning is housekeeping, and skipping a round costs nothing. *)
    match store.with_lock Layout.lock ~f:(fun () -> ()) with
    | Ok None -> "declined"
    | Ok (Some ()) -> "acquired twice"
    | Error msg -> msg
  end in
  Printf.printf "while held, a second attempt: %s\n"
    (match held with Ok (Some s) -> s | Ok None -> "never acquired" | Error m -> m);
  Printf.printf "once released, it can be taken: %b\n"
    (store.with_lock Layout.lock ~f:(fun () -> ()) = Ok (Some ()));
  [%expect {|
    while held, a second attempt: declined
    once released, it can be taken: true |}];;

let%expect_test "pruning reclaims by age" =
  let root = root () in
  let cache = Cache.create ~store:(Superbol_platform.Cache_store.create ~root ()) () in
  ignore (put cache ~key:(key "a") "parsed a");
  begin match Cache.prune cache ~policy:(Types.Bounded { max_age = Some 10_000.; max_size = None }) with
    | Ok removed -> Printf.printf "nothing is that old yet: %d\n" removed
    | Error _ -> print_endline "prune: failed"
  end;
  get cache ~key:(key "a");
  begin match Cache.prune cache ~policy:(Types.Bounded { max_age = Some 0.; max_size = None }) with
    | Ok removed -> Printf.printf "everything is older than nothing: %d\n" removed
    | Error _ -> print_endline "prune: failed"
  end;
  get cache ~key:(key "a");
  Printf.printf "and its value went too: %b\n"
    (match Cache.stats cache with
     | Ok { stats_objects = 0; _ } -> true
     | _ -> false);
  [%expect {|
    nothing is that old yet: 0
    hit: parsed a
    everything is older than nothing: 1
    miss
    and its value went too: true |}];;

let%expect_test "a shared directory is left writable by the group" =
  let root = root () in
  let cache = Cache.create ~store:(Superbol_platform.Cache_store.create ~root ()) () in
  ignore (put cache ~key:(key "a") "parsed a");
  let mode path = (Unix.stat (Filename.concat root path)).st_perm in
  (* Group-writable so that everyone sharing the cache can publish and prune,
     and set-group-ID so that what they create stays in the group.  No sticky
     bit: pruning has to reach entries it does not own. *)
  Printf.printf "directory: %o\n" (mode Layout.manifests);
  Printf.printf "entry is group-writable: %b\n"
    (mode (Layout.manifest (key "a")) land 0o060 = 0o060);
  [%expect {|
    directory: 2770
    entry is group-writable: true |}];;
