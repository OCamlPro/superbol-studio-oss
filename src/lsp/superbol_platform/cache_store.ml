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


(** A {!Superbol_cache.Store} backed by the file-system.

    This is where the cache's I/O actually happens; the library itself performs
    none, so that [cobol_lsp] keeps linking against a platform that cannot touch
    a disk.

    The implementation assumes as little as it can of the file-system holding
    the cache, because that may well be a network mount --- a build server, or a
    network home directory.  In particular it never replaces an existing file:
    publishing stages the item next door and then {e links} it into place, so
    that a reader sees either nothing or the whole item, and so that two
    instances publishing the same item at once cannot damage each other.  [link]
    failing with [EEXIST] is the ordinary outcome of losing that race, not an
    error.

    It also never relies on [flock], whose behaviour over NFS and SMB cannot be
    counted on; the pruner's lock is an exclusively-created file that a later
    instance may break once it has gone stale.

    {2 Sharing between users}

    Directories are created set-group-ID and group-writable, so that everyone in
    the owning group can publish, refresh and reclaim entries --- including ones
    another user wrote.  The sticky bit is deliberately {e not} set: pruning has
    to be able to remove entries it does not own.

    Bear in mind what that permits: values are read back with {!Marshal}, so
    anyone who can write to the directory can run code in everybody else's
    process.  A shared cache directory is only ever sound between users who
    already trust one another. *)

open Ez_file.V1
open Superbol_cache.Store.TYPES

(** Group-writable, and set-group-ID so that what is created under a shared
    directory stays in its group. *)
let dir_perm = 0o2770
let file_perm = 0o660

(** How long a lock left behind by a process that died is honoured before a
    later instance breaks it. *)
let lock_timeout = 600.

let protect f = try Ok (f ()) with
  | Unix.Unix_error (e, op, arg) ->
      Error (Printf.sprintf "%s %s: %s" op arg (Unix.error_message e))
  | Sys_error msg ->
      Error msg

let safe_unlink f =
  try Unix.unlink f with Unix.Unix_error _ -> ()

let safe_close fd =
  try Unix.close fd with Unix.Unix_error _ -> ()

(* Modes are forced rather than left to the caller's umask, which would
   otherwise keep the group out of a directory meant to be shared. *)
let force_perm path perm =
  try Unix.chmod path perm with Unix.Unix_error _ -> ()

let rec mkdir_p dir =                             (* assumes slashified [dir] *)
  if dir <> "" && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir dir_perm with Unix.Unix_error (EEXIST, _, _) -> ());
    force_perm dir dir_perm
  end

let write_whole_file path contents =
  let fd = EzFile.open_fd path [O_WRONLY; O_CREAT; O_EXCL] file_perm in
  Fun.protect ~finally:(fun () -> safe_close fd)
    begin fun () ->
      let oc = Unix.out_channel_of_descr fd in
      output_string oc contents;
      flush oc;
      (* Without this, a server crash can lose an item a reader has already been
         told about. *)
      try Unix.fsync fd with Unix.Unix_error _ -> ()
    end;
  force_perm path file_perm

(* Unique to this process, and retried should an instance that died have left
   the name behind: [write_whole_file] creates exclusively, so a collision is
   reported rather than silently overwriting. *)
let rec stage ~dir ~contents attempts =
  let staged =
    Printf.sprintf "%s/%d-%06x" dir (Unix.getpid ())
      (Random.bits () land 0xFFFFFF)
  in
  try
    write_whole_file staged contents;
    staged
  with Unix.Unix_error (EEXIST, _, _) when attempts > 0 ->
    stage ~dir ~contents (attempts - 1)

(* --- *)

let create ~root () =
  let path name = if name = "" then root else Filename.concat root name in
  let list name =
    protect begin fun () ->
      let dir = path name in
      if Sys.file_exists dir && Sys.is_directory dir
      then Array.to_list (Sys.readdir dir)
      else []
    end
  in
  let stat name =
    protect begin fun () ->
      match Unix.stat (path name) with
      | { st_kind = S_REG; st_size; st_mtime; _ } ->
          Some { item_size = Int64.of_int st_size; item_last_hit = st_mtime }
      | _ | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) ->
          None
    end
  in
  let read name =
    protect begin fun () ->
      let file = path name in
      if Sys.file_exists file then Some (EzFile.read_file file) else None
    end
  in
  let publish ~path:name ~contents ~replace =
    let target = path name in
    if Sys.file_exists target && not replace then Ok () else
      protect begin fun () ->
        mkdir_p (Filename.dirname target);
        mkdir_p (path Superbol_cache.Layout.tmp);
        let staged = stage ~dir:(path Superbol_cache.Layout.tmp) ~contents 100 in
        Fun.protect ~finally:(fun () -> safe_unlink staged)
          begin fun () ->
            if replace then
              (* [rename] replaces atomically on POSIX.  Where it will not
                 replace at all -- some Windows shares -- the target is taken
                 out of the way first, which leaves a window in which a reader
                 finds nothing and simply misses. *)
              try Unix.rename staged target with
              | Unix.Unix_error _ ->
                  safe_unlink target;
                  Unix.link staged target
            else
              try Unix.link staged target with
              | Unix.Unix_error (EEXIST, _, _) ->
                  ()                    (* lost the race; the item is there *)
              | Unix.Unix_error _ ->
                  (* Hard links are not available everywhere, notably on some
                     Windows shares.  Renaming is sound here only because the
                     target was just seen to be absent. *)
                  if not (Sys.file_exists target) then Unix.rename staged target
          end
      end
  in
  let with_lock name ~f =
    let lock = path name in
    let acquire () =
      Unix.openfile lock [O_WRONLY; O_CREAT; O_EXCL] file_perm
    in
    let stale () = match Unix.stat lock with
      | { st_mtime; _ } ->
          Unix.gettimeofday () -. st_mtime > lock_timeout
      | exception Unix.Unix_error _ ->
          false
    in
    protect begin fun () ->
      mkdir_p (Filename.dirname lock);
      let fd = match acquire () with
        | fd -> Some fd
        | exception Unix.Unix_error (EEXIST, _, _) ->
            (* Left behind by an instance that died, most likely. *)
            if not (stale ()) then None else begin
              safe_unlink lock;
              try Some (acquire ()) with Unix.Unix_error _ -> None
            end
      in
      match fd with
      | None -> None
      | Some fd ->
          Fun.protect (fun () -> Some (f ()))
            ~finally:begin fun () ->
              safe_close fd;
              safe_unlink lock;
            end
    end
  in
  (* Earlier versions of SuperBOL kept the whole of a project's cache in a
     single file at this very path.  Left in place, it would stop the directory
     from ever being created, and caching would quietly never work again. *)
  begin match Unix.stat root with
    | { st_kind = S_REG; _ } -> safe_unlink root
    | _ | exception Unix.Unix_error _ -> ()
  end;
  mkdir_p root;
  (* Settle, once and for the whole process, whether case distinguishes paths:
     this is the only place the volume actually holding the cache is known. *)
  Superbol_cache.Witness.set_case_insensitive (Case.insensitive ~root);
  {
    read; publish; list; stat; with_lock;
    touch = begin fun name ->
      try Unix.utimes (path name) 0. 0. with     (* 0., 0. means "just now" *)
      | Unix.Unix_error _ -> ()          (* someone else's, and that is fine *)
    end;
    remove = begin fun name ->
      safe_unlink (path name)   (* on error: held open, or not ours to remove *)
    end;
    now = Unix.gettimeofday;
  }
