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


(** Whether the host tells two paths differing only in case apart.

    Windows never does.  Elsewhere it is a property of the volume rather than of
    the system --- macOS is case-insensitive by default but need not be, and a
    Linux host may well have a share mounted that is not --- so it is settled by
    trying it rather than by guessing from the system name.

    The probe looks at the volume holding the cache, which is not necessarily
    the one holding the sources whose paths end up in witnesses.  Those are
    normally the same, and a wrong answer costs a spurious miss rather than a
    wrong hit: two spellings of one path simply fail to agree. *)

let probe ~root =
  let lower = Filename.concat root ".caseprobe" in
  let upper = Filename.concat root ".CASEPROBE" in
  match Unix.openfile lower Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o600 with
  | fd ->
      Fun.protect
        ~finally:begin fun () ->
          (try Unix.close fd with Unix.Unix_error _ -> ());
          (try Unix.unlink lower with Unix.Unix_error _ -> ())
        end
        (fun () -> Sys.file_exists upper)
  | exception (Unix.Unix_error _ | Sys_error _) ->
      false                        (* cannot tell; assume case is significant *)

let insensitive ~root =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> true
  | _ -> probe ~root
