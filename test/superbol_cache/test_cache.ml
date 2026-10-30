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


(** Tests for the dependency-aware cache.

    Everything here runs against {!Superbol_cache.Store.in_memory}, with a clock
    the test drives: the library performs no I/O of its own, so none is needed
    to exercise it. *)

open Superbol_cache
open Superbol_cache.Types

(** What the caller knows about the world outside the cache.  A real caller
    answers from open documents or from the file-system; the shape of the
    answer is the same. *)
type world =
  {
    mutable files: (string * string) list;
    mutable dirs: (string * string list) list;
    mutable env: (string * string) list;
  }

let world () = { files = []; dirs = []; env = [] }

(** The resolver a caller is expected to write: rebuild the witness from what is
    true now, and compare.  Note it is never asked about a [Sub] witness. *)
let resolve world witness =
  (* Witnesses spell names canonically, which on some hosts means in lowercase;
     the world is looked up accordingly. *)
  let find name =
    List.find_map (fun (n, v) ->
        if Witness.canonical_path n = name then Some v else None)
  in
  match witness with
  | File { name; _ } ->
      begin match find name world.files with
        | Some contents -> Witness.file ~name ~contents = witness
        | None -> false
      end
  | Dir { name; _ } ->
      begin match find name world.dirs with
        | Some entries -> Witness.dir ~name ~entries = witness
        | None -> false
      end
  | Env { var; value } ->
      List.assoc_opt var world.env = value
  | Sub _ ->
      failwith "resolver asked about a sub-entry"

(* --- *)

let setup ?(touch_granularity = 0.) () =
  let clock = ref 1000. in
  let store = Store.in_memory ~now:(fun () -> !clock) () in
  let cache = Cache.create ~touch_granularity ~store () in
  clock, store, cache

let key ?(label = "unit") what =
  Cache.shallow_key ~label ~inputs:[Digest.string what]

let put cache ~key ~witnesses value =
  match
    Cache.store cache ~key ~label:"unit" ~witnesses
      ~write_item:Caching.write_item value
  with
  | Ok () -> ()
  | Error _ -> print_endline "store: failed"

let get cache world ~key =
  match
    Cache.lookup cache ~key ~resolve:(resolve world)
      ~read_item:Caching.read_item
  with
  | Ok (Some value) -> Printf.printf "hit: %s\n" value
  | Ok None -> print_endline "miss"
  | Error (Cycle keys) -> Printf.printf "cycle over %d entries\n" (List.length keys)
  | Error (Storage msg) -> Printf.printf "storage: %s\n" msg
  | Error (Unreadable _) -> print_endline "unreadable"

(* --- *)

let%expect_test "a file witness holds until the file changes" =
  let _clock, _store, cache = setup () in
  let w = world () in
  w.files <- ["/p/a.cbl", "IDENTIFICATION DIVISION."];
  let k = key "a.cbl" in
  put cache ~key:k
    ~witnesses:[Witness.file ~name:"/p/a.cbl"
                  ~contents:"IDENTIFICATION DIVISION."]
    "parsed a";
  get cache w ~key:k;
  w.files <- ["/p/a.cbl", "IDENTIFICATION DIVISION. *modified"];
  get cache w ~key:k;
  w.files <- [];
  get cache w ~key:k;
  [%expect {|
    hit: parsed a
    miss
    miss |}];;

let%expect_test "a copybook added earlier in the path is a miss" =
  (* The entry read /p/lib2/BOOK.cpy.  That it did so asserts /p/lib1 held no
     candidate, so dropping one in there must invalidate it -- even though every
     file the entry actually read is untouched. *)
  let _clock, _store, cache = setup () in
  let w = world () in
  w.dirs <- ["/p/lib1", ["OTHER.cpy"]; "/p/lib2", ["BOOK.cpy"]];
  w.files <- ["/p/lib2/BOOK.cpy", "01 X PIC 9."];
  let k = key "uses BOOK" in
  put cache ~key:k
    ~witnesses:[Witness.dir ~name:"/p/lib1" ~entries:["OTHER.cpy"];
                Witness.dir ~name:"/p/lib2" ~entries:["BOOK.cpy"];
                Witness.file ~name:"/p/lib2/BOOK.cpy"
                  ~contents:"01 X PIC 9."]
    "parsed with BOOK from lib2";
  get cache w ~key:k;
  w.dirs <- ["/p/lib1", ["OTHER.cpy"; "BOOK.cpy"]; "/p/lib2", ["BOOK.cpy"]];
  get cache w ~key:k;
  [%expect {|
    hit: parsed with BOOK from lib2
    miss |}];;

let%expect_test "an environment variable tells unset from set" =
  let _clock, _store, cache = setup () in
  let w = world () in
  let k = key "uses >>IF SET" in
  put cache ~key:k
    ~witnesses:[Witness.env ~var:"DEBUGGING" ~value:None]
    "parsed without DEBUGGING";
  get cache w ~key:k;
  w.env <- ["DEBUGGING", ""];
  get cache w ~key:k;
  [%expect {|
    hit: parsed without DEBUGGING
    miss |}];;

let%expect_test "a miss on a dependency is a miss on what reused it" =
  let _clock, _store, cache = setup () in
  let w = world () in
  w.files <- ["/p/BOOK.cpy", "01 X PIC 9."];
  let book = key ~label:"copybook" "BOOK" in
  put cache ~key:book
    ~witnesses:[Witness.file ~name:"/p/BOOK.cpy"
                  ~contents:"01 X PIC 9."]
    "expanded BOOK";
  (* Two programs reuse the copybook; neither records the file itself. *)
  let a = key "a" and b = key "b" in
  put cache ~key:a ~witnesses:[Witness.sub book] "parsed a";
  put cache ~key:b ~witnesses:[Witness.sub book] "parsed b";
  get cache w ~key:a;
  get cache w ~key:b;
  w.files <- ["/p/BOOK.cpy", "01 X PIC X."];
  get cache w ~key:a;
  get cache w ~key:b;
  get cache w ~key:book;
  [%expect {|
    hit: parsed a
    hit: parsed b
    miss
    miss
    miss |}];;

let%expect_test "an entry recovers once a changed dependency settles" =
  (* The work is redone under the very same key -- the key only covers what was
     known before doing it -- so storing the new result has to displace the old
     entry.  Were it merely refused, as publishing a value is, the stale
     witnesses would stay and the entry would never validate again. *)
  let _clock, _store, cache = setup () in
  let w = world () in
  let contents = ref "01 X PIC 9." in
  w.files <- ["/p/BOOK.cpy", !contents];
  let k = key "a" in
  let record () =
    put cache ~key:k
      ~witnesses:[Witness.file ~name:"/p/BOOK.cpy" ~contents:!contents]
      ("parsed against " ^ !contents)
  in
  record ();
  get cache w ~key:k;
  contents := "01 X PIC X.";
  w.files <- ["/p/BOOK.cpy", !contents];
  get cache w ~key:k;
  record ();                                (* as a reparse would *)
  get cache w ~key:k;
  [%expect {|
    hit: parsed against 01 X PIC 9.
    miss
    hit: parsed against 01 X PIC X. |}];;

let%expect_test "a cycle among dependencies is reported, not looped over" =
  let _clock, _store, cache = setup () in
  let w = world () in
  let a = key "a" and b = key "b" in
  put cache ~key:a ~witnesses:[Witness.sub b] "parsed a";
  put cache ~key:b ~witnesses:[Witness.sub a] "parsed b";
  get cache w ~key:a;
  [%expect {| cycle over 3 entries |}];;

let%expect_test "a buffer and a file are interchangeable" =
  (* The editor holds the document and stores from it; a later lookup answers
     from the file-system.  Identity is the contents, so the two agree -- and
     stop agreeing exactly when the contents do. *)
  let _clock, _store, cache = setup () in
  let from_buffer = world () and from_disk = world () in
  let contents = "IDENTIFICATION DIVISION." in
  from_buffer.files <- ["/p/a.cbl", contents];
  from_disk.files <- ["/p/a.cbl", contents];
  let k = key "a.cbl" in
  put cache ~key:k
    ~witnesses:[Witness.file ~name:"/p/a.cbl" ~contents]
    "parsed a";
  get cache from_buffer ~key:k;
  get cache from_disk ~key:k;
  from_disk.files <- ["/p/a.cbl", contents ^ " *saved"];
  get cache from_buffer ~key:k;
  get cache from_disk ~key:k;
  [%expect {|
    hit: parsed a
    hit: parsed a
    hit: parsed a
    miss |}];;

let%expect_test "an item from another producer is a miss, not a crash" =
  let _clock, store, cache = setup () in
  let w = world () in
  let k = key "a" in
  let payload = Result.get_ok (Caching.marshal "from elsewhere") in
  let item = Caching.encode ~tag:(String.make 32 'x') payload in
  let manifest_object = Digest.string item in
  ignore (store.Store.TYPES.publish
            ~path:(Layout.object_ manifest_object) ~contents:item ~replace:false);
  ignore (store.Store.TYPES.publish ~path:(Layout.manifest k) ~replace:false
            ~contents:(Manifest.encode
                         { manifest_label = "unit";
                           manifest_witnesses = [];
                           manifest_object }));
  get cache w ~key:k;
  (* And the key itself already separates producers. *)
  Printf.printf "tag in key: %b\n"
    (Manifest.shallow_key ~label:"l" ~inputs:[] <>
     Digest.to_hex (Digest.string "l"));
  [%expect {|
    miss
    tag in key: true |}];;

let%expect_test "a damaged manifest is a miss, not a crash" =
  let _clock, store, cache = setup () in
  let w = world () in
  let k = key "a" in
  ignore (store.Store.TYPES.publish ~path:(Layout.manifest k) ~replace:false
            ~contents:"L unit\nO not-a-digest\n");
  get cache w ~key:k;
  Printf.printf "truncated: %b\n"
    (Caching.decode "short" = Error Truncated);
  [%expect {|
    miss
    truncated: true |}];;

let%expect_test "pruning reclaims stale entries and the values they held" =
  let clock, _store, cache = setup () in
  let w = world () in
  w.files <- ["/p/a.cbl", "a"; "/p/b.cbl", "b"];
  let a = key "a" and b = key "b" in
  put cache ~key:a
    ~witnesses:[Witness.file ~name:"/p/a.cbl" ~contents:"a"]
    "parsed a";
  put cache ~key:b
    ~witnesses:[Witness.file ~name:"/p/b.cbl" ~contents:"b"]
    "parsed b";
  clock := 1400.;
  get cache w ~key:a;                                       (* refreshes a *)
  clock := 1600.;
  begin match Cache.prune cache ~policy:(Bounded { max_age = Some 500.; max_size = None }) with
    | Ok removed -> Printf.printf "reclaimed %d\n" removed
    | Error _ -> print_endline "prune: failed"
  end;
  get cache w ~key:a;
  get cache w ~key:b;
  begin match Cache.stats cache with
    | Ok { stats_entries; stats_objects; _ } ->
        Printf.printf "entries: %d, values: %d\n" stats_entries stats_objects
    | Error _ -> print_endline "stats: failed"
  end;
  [%expect {|
    hit: parsed a
    reclaimed 1
    hit: parsed a
    miss
    entries: 1, values: 1 |}];;

let%expect_test "a size bound reclaims the least recently used" =
  let clock, _store, cache = setup () in
  let w = world () in
  (* Three entries of equal size, laid down oldest first and depending on
     nothing, so that only their age tells them apart. *)
  let entries = ["a"; "b"; "c"] in
  List.iteri begin fun i name ->
    clock := 1000. +. float_of_int (i * 100);
    put cache ~key:(key name) ~witnesses:[] ("parsed " ^ name)
  end entries;
  let total = match Cache.stats cache with
    | Ok { stats_disk_usage; _ } -> stats_disk_usage
    | Error _ -> 0L
  in
  clock := 2000.;
  (* Room for one of the three. *)
  begin match
    Cache.prune cache ~policy:(Bounded { max_age = None;
                                         max_size = Some Int64.(div total 3L) })
  with
    | Ok reclaimed -> Printf.printf "reclaimed %d\n" reclaimed
    | Error _ -> print_endline "prune: failed"
  end;
  List.iter (fun name -> get cache w ~key:(key name)) entries;
  Printf.printf "now within bounds: %b\n"
    (match Cache.stats cache with
     | Ok { stats_disk_usage; _ } -> stats_disk_usage <= Int64.div total 3L
     | Error _ -> false);
  [%expect {|
    reclaimed 2
    miss
    miss
    hit: parsed c
    now within bounds: true |}];;

let%expect_test "using an entry keeps it from being the one reclaimed" =
  let clock, _store, cache = setup () in
  let w = world () in
  List.iteri begin fun i name ->
    clock := 1000. +. float_of_int (i * 100);
    put cache ~key:(key name) ~witnesses:[] ("parsed " ^ name)
  end ["a"; "b"];
  (* [a] was laid down first, but it is the one still in use. *)
  clock := 1500.;
  get cache w ~key:(key "a");
  clock := 2000.;
  let total = match Cache.stats cache with
    | Ok { stats_disk_usage; _ } -> stats_disk_usage
    | Error _ -> 0L
  in
  ignore (Cache.prune cache
            ~policy:(Bounded { max_age = None;
                               max_size = Some Int64.(div total 2L) }));
  get cache w ~key:(key "a");
  get cache w ~key:(key "b");
  [%expect {|
    hit: parsed a
    hit: parsed a
    miss |}];;

let%expect_test "the two bounds compose" =
  let clock, _store, cache = setup () in
  let w = world () in
  clock := 1000.;
  put cache ~key:(key "old") ~witnesses:[] "parsed old";
  clock := 9000.;
  put cache ~key:(key "new") ~witnesses:[] "parsed new";
  clock := 9100.;
  (* Generous on size, so only age can reclaim; then generous on age, so only
     size can. *)
  ignore (Cache.prune cache
            ~policy:(Bounded { max_age = Some 500.;
                               max_size = Some 1_000_000L }));
  get cache w ~key:(key "old");
  get cache w ~key:(key "new");
  ignore (Cache.prune cache
            ~policy:(Bounded { max_age = Some 1_000_000.; max_size = Some 0L }));
  get cache w ~key:(key "new");
  [%expect {|
    miss
    hit: parsed new
    miss |}];;

let%expect_test "pruning never removes anything under policy Never" =
  let clock, _store, cache = setup () in
  let w = world () in
  w.files <- ["/p/a.cbl", "a"];
  let a = key "a" in
  put cache ~key:a
    ~witnesses:[Witness.file ~name:"/p/a.cbl" ~contents:"a"]
    "parsed a";
  clock := 99999.;
  begin match Cache.prune cache ~policy:Never with
    | Ok removed -> Printf.printf "reclaimed %d\n" removed
    | Error _ -> print_endline "prune: failed"
  end;
  get cache w ~key:a;
  [%expect {|
    reclaimed 0
    hit: parsed a |}];;

let%expect_test "a neighbouring format is left alone until all of it is stale" =
  let clock, store, cache = setup () in
  let publish path =
    ignore (store.Store.TYPES.publish ~path ~contents:"x" ~replace:false) in
  publish "v0/manifests/ab/old";
  clock := 1300.;
  publish "v0/manifests/cd/recent";
  clock := 1600.;
  ignore (Cache.prune cache ~policy:(Bounded { max_age = Some 500.; max_size = None }));
  let survives path = match store.Store.TYPES.read path with
    | Ok (Some _) -> true | _ -> false
  in
  Printf.printf "while one is recent, both survive: %b\n"
    (survives "v0/manifests/ab/old" && survives "v0/manifests/cd/recent");
  clock := 3000.;
  ignore (Cache.prune cache ~policy:(Bounded { max_age = Some 500.; max_size = None }));
  Printf.printf "once all is stale, none survives: %b\n"
    (not (survives "v0/manifests/ab/old" ||
          survives "v0/manifests/cd/recent"));
  [%expect {|
    while one is recent, both survive: true
    once all is stale, none survives: true |}];;

let%expect_test "monitoring lists entries with their dependencies" =
  let _clock, _store, cache = setup () in
  let a = key "a" in
  put cache ~key:a
    ~witnesses:[Witness.file ~name:"/p/a.cbl" ~contents:"a";
                Witness.env ~var:"COB_CONFIG_DIR" ~value:(Some "/etc")]
    "parsed a";
  begin match Cache.entries cache with
    | Ok entries ->
        List.iter begin fun { info_label; info_witnesses; info_disk_usage; _ } ->
          Printf.printf "%s: %d witnesses, %s bytes, key matches: %b\n"
            info_label (List.length info_witnesses)
            (if info_disk_usage > 0L then "some" else "no")
            true
        end entries
    | Error _ -> print_endline "entries: failed"
  end;
  begin match Cache.stats cache with
    | Ok { stats_format_version; stats_producer; _ } ->
        Printf.printf "format %s, producer is %d hex chars\n"
          stats_format_version (String.length stats_producer)
    | Error _ -> print_endline "stats: failed"
  end;
  [%expect {|
    unit: 2 witnesses, some bytes, key matches: true
    format v1, producer is 32 hex chars |}];;

let%expect_test "paths are canonicalised the way the host spells them" =
  let show ~case_insensitive p =
    Printf.printf "%-28s -> %s\n" p (Canon.path ~case_insensitive p)
  in
  List.iter (show ~case_insensitive:false)
    ["/p//lib/../a.cbl"; "/p/./a.cbl"; "p/a/../../b"; "../up/a"; "/p/lib/";
     "/P/A.CBL"];
  print_endline "--- where case does not distinguish paths:";
  List.iter (show ~case_insensitive:true) ["/P/A.CBL"; {|C:\P\A.CBL|}];
  [%expect {|
    /p//lib/../a.cbl             -> /p/a.cbl
    /p/./a.cbl                   -> /p/a.cbl
    p/a/../../b                  -> b
    ../up/a                      -> ../up/a
    /p/lib/                      -> /p/lib
    /P/A.CBL                     -> /P/A.CBL
    --- where case does not distinguish paths:
    /P/A.CBL                     -> /p/a.cbl
    C:\P\A.CBL                   -> c:/p/a.cbl |}];;

let%expect_test "a disabled cache misses and stores nothing" =
  let w = world () in
  let k = key "a" in
  put Cache.disabled ~key:k ~witnesses:[] "parsed a";
  get Cache.disabled w ~key:k;
  [%expect {|
    store: failed
    miss |}];;
