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

let () =
  GoblintCil.initCIL ()

let preprocess_c_file include_files c_file c_file_pp =
  let null = if Sys.win32 then "NUL" else "/dev/null" in
  let c_null_in = open_in null in
  let c_null_out = open_out null in
  let null_in = Unix.descr_of_in_channel c_null_in in
  let null_out = Unix.descr_of_out_channel c_null_out in
  let args =
    [ "cpp";
      "-D"; "GENSTUBS";
      "-D"; "NOT_IMPLEMENTED=__attribute__((NOT_IMPLEMENTED))";
      "-D"; "NO_CONSTR=__attribute__((NO_CONSTR))";
      "-D"; "NO_DESTR=__attribute__((NO_DESTR))";
      "-D"; "CONSTANT(v)=__attribute__((CONSTANT(v)))";
      "-D"; "VA(t,l)=__attribute__((VA(sizeof(t),l)))";
      "-D"; "VA_N(t,l)=__attribute__((VA_N(sizeof(t),l)))";
      "-D"; "VA_PROTO(s,n,...)=__attribute__((VA_PROTO(s,n,sizeof(void (*)(__VA_ARGS__)))))";
      "-D"; "VA_FORMAT=__attribute__((VA_FORMAT))";
      c_file; "-o"; c_file_pp ] @
    List.rev (List.fold_left (fun args include_file ->
        include_file :: "-I" :: args
      ) [] include_files)
  in
  let status_opt =
    try
      let pid =
        Unix.create_process_env "cpp" (Array.of_list args)
          (Unix.environment ()) null_in null_out null_out
      in
      let _pid, status = Unix.waitpid [ (* WNOHANG *) ] pid in
      Some (status)
    with _ ->
      None
  in
  close_out c_null_out;
  close_in c_null_in;
  match status_opt with
  | Some (WEXITED (code)) ->
      code = 0
  | Some (WSIGNALED (signal)) ->
      Printf.eprintf "cpp killed by signal %d\n" signal; false
  | Some (WSTOPPED (signal)) ->
      Printf.eprintf "cpp stopped by signal %d\n" signal; false
  | None ->
      Printf.eprintf "could not start cpp\n"; false

let process_c_file c_file_pp =
  try
    let ci_file = GoblintCil.Frontc.parse c_file_pp () in
    let is_root = function
      | GoblintCil.GVarDecl (vi, _loc) ->
          not (String.starts_with ~prefix:"__" vi.vname)
      | GType (_ti, _loc) -> (* Typedef ttype tname *)
          true
      | GCompTag (_ci, _loc) -> (* Full struct declaration *)
          true
      | GCompTagDecl (_ci, _loc) -> (* Forward struct declaration *)
          true
      | GEnumTag (_ei, _loc) -> (* Full enum declaration *)
          true
      | GEnumTagDecl (_ei, _loc) -> (* Forward enum declaration *)
          true
      | _ ->
          false
    in
    GoblintCil.RmUnused.removeUnused ~isRoot:is_root ci_file;
    Some (ci_file)
  with _ ->
    None

let parse_c_file c_file =
  let c_file_pp = Filename.temp_file (c_file ^ ".") ".pp" in
  let res = preprocess_c_file [ (* include paths *) ] c_file c_file_pp in
  let res' =
    if res then process_c_file c_file_pp
    else None
  in
  Sys.remove c_file_pp;
  res'
