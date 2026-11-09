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

(** Tests for the loading of COBOL configurations through a platform, and its
    recording.

    Everything here runs against an in-memory file system. *)

open Cobol_common.Platform.TYPES

let files =
  [
    "/conf/my.conf", "include \"inc.conf\"\nname: \"default\"\n";
    "/conf/inc.conf", "tab-width: 4\n";
    (* Comes earlier in the search path, but is not next to [my.conf]: *)
    "/elsewhere/inc.conf", "tab-width: 2\n";
  ]

(* Can be found, but not read. *)
let unreadable = "/conf/unreadable.conf"

let read_text_file file =
  match List.assoc_opt file files with
  | Some contents -> contents
  | None -> raise @@ Sys_error (file ^ ": cannot be read")

let stat_file ?follow:_ file =
  if List.mem_assoc file files || file = unreadable
  then Some { file_kind = Regular_file; file_mtime = 0.; file_size = 0 }
  else None

let platform =
  { Cobol_common.Platform.innocuous with
    getcwd = (fun () -> "/work");
    read_text_file;
    stat_file;
    read_text_file_in_path =
      Cobol_common.Platform.FS.read_text_file_in_path
        ~concat:(fun dir file -> dir ^ "/" ^ file)
        ~read_text_file ~stat_file:(fun path -> stat_file path) }

let search_path = ["/elsewhere"; "/conf"]

let load ~platform name =
  Cobol_config.from_dialect_n_file ~platform ~search_path @@
  Cobol_config.DIALECT.of_string name

let%expect_test "configuration files are looked up and read at once" =
  let { result = config, file; _ }: _ Cobol_common.Diagnostics.with_diags =
    load ~platform "my.conf" in
  Printf.printf "loaded: %s\n" (Option.value file ~default:"none");
  (* [inc.conf] is taken next to [my.conf]: *)
  Printf.printf "tab-width: %d\n" (Cobol_config.tab_stops config |> List.hd);
  begin match load ~platform "missing.conf" with
    | _ -> print_endline "missing: loaded?"
    | exception Cobol_config.ERROR (Missing_file (name, _)) ->
        Printf.printf "missing: %s\n" name
  end;
  begin match load ~platform "unreadable.conf" with
    | _ -> print_endline "unreadable: loaded?"
    | exception Sys_error msg -> Printf.printf "unreadable: %s\n" msg
  end;
  [%expect {|
    loaded: /conf/my.conf
    tab-width: 4
    missing: missing.conf
    unreadable: /conf/unreadable.conf: cannot be read |}];;

let%expect_test "configuration files read are recorded" =
  let recorder = Superbol_cache.Recorder.create ~platform in
  let platform = Superbol_cache.Recorder.platform recorder in
  ignore @@ load ~platform "my.conf";
  Superbol_cache.Recorder.witnesses recorder |>
  List.filter_map (function
      | Superbol_cache.Types.File { name; _ } -> Some name
      | _ -> None) |>
  List.sort String.compare |>
  List.iter print_endline;
  [%expect {|
    /conf/inc.conf
    /conf/my.conf |}];;
