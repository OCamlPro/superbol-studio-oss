(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
(* open EzFile.OP *)

let debug = ref false

module GLOBALS = struct
  let verbose _ = !debug
end
module ERROR = struct
  let raise fmt = Printf.kprintf failwith fmt
end


(* [BEGIN] The following part is similar to ocamlup:call.ml *)

let command ?on_error fmt =
  Printf.kprintf (fun cmd ->
      Printf.eprintf "%s\n%!" cmd;
      let retcode = Sys.command cmd in
      if retcode <> 0 then begin
        Printf.eprintf "  returned error %d\n%!" retcode;
        match on_error with
        | None -> ()
        | Some f -> f cmd retcode
      end
    ) fmt

let command_exn fmt =
  let on_error cmd retcode =
    ERROR.raise "external command %S failed with code %d" cmd retcode
  in
  command ~on_error fmt

let tmpfile () =
  Filename.temp_file "tmpfile" ".tmp"

let call
    ?(echo=false)
    ?(stdout = Unix.stdout)
    ?(stderr = Unix.stderr)
    args =
  if echo || GLOBALS.verbose 2 then
    Printf.eprintf "Calling %s\n%!" (String.concat " " args);
  let targs = Array.of_list args in
  let pid = Unix.create_process targs.(0) targs
      Unix.stdin stdout stderr in
  let rec iter () =
    match Unix.waitpid [] pid with
    | exception Unix.Unix_error (EINTR, _, _) -> iter ()
    | _pid, status -> (
      match status with
      | WEXITED 0 -> ()
      | _ ->
        ERROR.raise "Command '%s' exited with error code %s"
          (String.concat " " args)
          ( match status with
          | WEXITED n -> string_of_int n
          | WSIGNALED n -> Printf.sprintf "SIGNAL %d" n
          | WSTOPPED n -> Printf.sprintf "STOPPED %d" n ) )
  in
  iter ()

let call_stdout_file ?(stderr=false) ?file args =
  let tmpfile = match file with
    | None -> tmpfile ()
    | Some file -> file in
  let stdout = Unix.openfile tmpfile
      [ Unix.O_CREAT ; Unix.O_WRONLY ; Unix.O_TRUNC ] 0o644 in
  let stderr = if stderr then
      Some stdout
    else None
  in
  match call ~stdout ?stderr args with
  | () ->
      Unix.close stdout;
      tmpfile
  | exception exn ->
      let stdout = EzFile.read_file tmpfile in
      if GLOBALS.verbose 2 then
        Printf.eprintf "Stdout after error:\n%s\n" stdout;
      raise exn

let call_stdout_string ?stderr args =
  let file = call_stdout_file ?stderr args in
  let stdout = EzFile.read_file file in
  Sys.remove file;
  if GLOBALS.verbose 2 then
    Printf.eprintf "stdout:\n%s\n%!" stdout ;
  stdout

let call_stdout_lines ?stderr args =
  let file = call_stdout_file ?stderr args in
  let stdout = EzFile.read_lines file in
  Sys.remove file;
  let lines = Array.to_list stdout in
  if GLOBALS.verbose 2 then
    Printf.eprintf "stdout:\n%s\n%!"
      (String.concat "\n" lines);
  lines

(* [END] The preceeding part is similar to ocamlup:call.ml *)


let create_process ?stdin ?stdout ?stderr args =
  if GLOBALS.verbose 2 then
    Printf.eprintf "Calling %s\n%!" (String.concat " " args);
  let targs = Array.of_list args in
  let close_stdin = stdin <> None in
  let close_stdout = stdout <> None in
  let close_stderr = stderr <> None in
  let stdin = match stdin with
    | None -> Unix.stdin
    | Some file -> Unix.openfile file [ Unix.O_RDONLY ] 0o644
  in
  let stdout = match stdout with
    | None -> Unix.stdout
    | Some file ->
        Unix.openfile file
          [ Unix.O_CREAT ; Unix.O_WRONLY ; Unix.O_TRUNC ] 0o644
  in
  let stderr = match stderr with
    | None -> Unix.stderr
    | Some file ->
        Unix.openfile file
          [ Unix.O_CREAT ; Unix.O_WRONLY ; Unix.O_TRUNC ] 0o644
  in
  let pid = Unix.create_process targs.(0) targs Unix.stdin stdout stderr in
  if close_stdin then Unix.close stdin ;
  if close_stdout then Unix.close stdout ;
  if close_stderr then Unix.close stderr ;
  pid

let rec wait_pids () =
  try Unix.wait () with
  | Unix.Unix_error (EINTR, _, _) -> wait_pids ()
