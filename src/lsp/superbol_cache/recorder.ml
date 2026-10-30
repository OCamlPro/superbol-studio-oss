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


open Cobol_common.Platform.TYPES

module TYPES = struct

  type t =
    {
      rec_platform: platform;
      mutable rec_witnesses: Types.witness list;
      (* Directories are listed on the recorder's own account, over and above
         what the lookup itself does, so each one is listed at most once for the
         whole of a parse. *)
      rec_listed: (string, unit) Hashtbl.t;
    }

end
open TYPES

type t = TYPES.t

(* Copybook lookup paths are routinely relative -- the default one is literally
   ["."] -- and are resolved against the process's working directory.  A witness
   holding such a path would denote whichever directory a later session happens
   to be started from, so it is pinned here, while the working directory is
   still the one the lookup actually used. *)
let absolute t name =
  if Filename.is_relative name
  then Filename.concat (t.rec_platform.getcwd ()) name
  else name

let add t witness =
  if not (List.mem witness t.rec_witnesses) then
    t.rec_witnesses <- witness :: t.rec_witnesses

let witnesses { rec_witnesses; _ } = rec_witnesses

(* The directories a copybook lookup may consult.  [Copybook_finder] narrows
   this down to the one named by [libname] when there is one, and reads the
   whole of the lookup path otherwise; recording the union of the two is
   therefore never short of what was actually read, which is the direction that
   matters -- an extra witness costs a needless miss, a missing one gives a
   wrong hit. *)
let lookup_dirs ~lookup_config ?fromfile ?libname () =
  let named = match libname, fromfile with
    | None, _ -> []
    | Some (`Word d | `Alphanum d), Some file when Filename.is_relative d ->
        [Filename.concat (Filename.dirname file) d]
    | Some (`Word d | `Alphanum d), _ -> [d]
  in
  lookup_config.Cobol_common.Copybook.TYPES.lookup_path @ named

let record_dir t dir =
  if not (Hashtbl.mem t.rec_listed dir) then begin
    Hashtbl.replace t.rec_listed dir ();
    add t @@ Witness.dir ~name:(absolute t dir)
      ~entries:(t.rec_platform.list_dir dir)
  end

let create ~platform =
  { rec_platform = platform;
    rec_witnesses = [];
    rec_listed = Hashtbl.create 16 }

let platform t =
  let inner = t.rec_platform in
  { inner with
    read_text_file = begin fun file ->
      let contents = inner.read_text_file file in
      add t (Witness.file ~name:(absolute t file) ~contents);
      contents
    end;
    getenv_opt = begin fun var ->
      let value = inner.getenv_opt var in
      add t (Witness.env ~var ~value);
      value
    end;
    find_lib = begin fun ~lookup_config ?fromfile ?libname textname ->
      List.iter (record_dir t) (lookup_dirs ~lookup_config ?fromfile ?libname ());
      inner.find_lib ~lookup_config ?fromfile ?libname textname
    end }

(* --- *)

let resolver ~platform ?(contents_of = fun _ -> None) () =
  let listed = Hashtbl.create 16 in
  fun witness ->
    match (witness: Types.witness) with
    | File { name; _ } ->
        let contents = match contents_of name with
          | Some contents -> Some contents
          | None -> (try Some (platform.read_text_file name) with
              | Sys_error _ -> None)
        in
        begin match contents with
          | None -> false
          | Some contents -> Witness.file ~name ~contents = witness
        end
    | Dir { name; _ } ->
        let entries = match Hashtbl.find_opt listed name with
          | Some entries -> entries
          | None ->
              let entries = platform.list_dir name in
              Hashtbl.replace listed name entries;
              entries
        in
        Witness.dir ~name ~entries = witness
    | Env { var; value } ->
        platform.getenv_opt var = value
    | Sub _ ->
        false                     (* followed within the cache, never resolved *)
