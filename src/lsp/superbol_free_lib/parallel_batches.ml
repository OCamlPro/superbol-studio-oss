(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1

let supported =
  match Sys.backend_type with
  | Native | Bytecode -> true
  | Other _ -> false                                    (* js_of_ocaml *)

let no_banner_var = "SUPERBOL_NO_BANNER"

let max_batch_size = 25

let rec batches size = function
  | [] -> []
  | l ->
      let rec take i acc = function
        | x :: rest when i < size -> take (succ i) (x :: acc) rest
        | rest -> List.rev acc, rest
      in
      let batch, rest = take 0 [] l in
      batch :: batches size rest

type process =
  {
    pid: int;
    batch: string list;
    out: string;                       (* files holding the output, until *)
    err: string;                       (* the process ends *)
  }

let pp_batch ppf batch =
  match batch with
  | [file] -> Pretty.print ppf "`%s'" file
  | files -> Pretty.print ppf "%d files" (List.length files)

let remove_file f =
  try Sys.remove f with Sys_error _ -> ()

let start ~env ~argv batch =
  let out = Filename.temp_file "superbol-batch" ".out"
  and err = Filename.temp_file "superbol-batch" ".err" in
  let open_out f = Unix.openfile f [O_WRONLY; O_TRUNC; O_CLOEXEC] 0o600 in
  let fd_out = open_out out and fd_err = open_out err in
  let argv = Array.of_list (argv @ batch) in
  match
    Fun.protect ~finally:(fun () -> Unix.close fd_out; Unix.close fd_err) @@
    fun () -> Unix.create_process_env argv.(0) argv env Unix.stdin fd_out fd_err
  with
  | pid ->
      Some { pid; batch; out; err }
  | exception (Unix.Unix_error _ as e) ->
      remove_file out;
      remove_file err;
      Cobol_common.record_failure ();
      Pretty.error "@[<hov>Could@ not@ start@ a@ process@ for@ %a:@ %a@]@."
        pp_batch batch Pretty.failure e;
      None

let copy_n_remove file oc =
  output_string oc (EzFile.read_file file);
  flush oc;
  remove_file file

let finish { batch; out; err; _ } (status: Unix.process_status) =
  Common_args.clear_progress ();
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  copy_n_remove out stdout;
  copy_n_remove err stderr;
  match status with
  | WEXITED code ->
      if code land 1 <> 0 then Cobol_common.record_errors ();
      if code > 1 then Cobol_common.record_failure ()
  | WSIGNALED _ | WSTOPPED _ ->
      Cobol_common.record_failure ();
      Pretty.error "@[<hov>The@ process@ for@ %a@ was@ interrupted@]@."
        pp_batch batch

let abort { pid; out; err; _ } =
  (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
  (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
  remove_file out;
  remove_file err

(* [Unix.wait] is not available on Windows: processes are polled instead. *)
let poll_interval = 0.02

let rec waitpid_nohang pid =
  match Unix.waitpid [WNOHANG] pid with
  | exception Unix.Unix_error (EINTR, _, _) -> waitpid_nohang pid
  | 0, _ -> None
  | _, status -> Some status

let run_batches ~jobs ~progress ~argv batches =
  (* Outputs of processes go to files, but are copied to those of this one:
     they are styled and laid out the same. *)
  let env =
    let set = [no_banner_var, "1";
               Globals.colors_var, Pretty.colors_spec ();
               Globals.margins_var, Pretty.margins_spec ()] in
    let is_set binding =
      List.exists (fun (var, _) -> String.starts_with ~prefix:(var ^ "=") binding)
        set
    in
    Array.of_list @@
    List.map (fun (var, value) -> var ^ "=" ^ value) set @
    List.filter (fun binding -> not (is_set binding)) @@
    Array.to_list (Unix.environment ()) in
  let pending = Queue.of_seq (List.to_seq batches) in
  let running = ref [] in
  let num_files = List.fold_left (fun n b -> n + List.length b) 0 batches
  and num_processed = ref 0 in
  let report_progress () =
    match List.length !running with
    | 0 -> Common_args.pretty_progress progress
             "Processed@ %d/%d@ files" !num_processed num_files
    | n -> Common_args.pretty_progress progress
             "Processed@ %d/%d@ files@ (%d@ process%s@ running)"
             !num_processed num_files n (if n = 1 then "" else "es")
  in
  let rec fill () =
    if List.length !running < jobs && not (Queue.is_empty pending) then begin
      Option.iter (fun p -> running := p :: !running) @@
      start ~env ~argv (Queue.pop pending);
      fill ()
    end
  in
  let step () =
    let ended, still_running =
      List.partition_map begin fun p -> match waitpid_nohang p.pid with
        | Some status -> Left (p, status)
        | None -> Right p
      end !running
    in
    running := still_running;
    List.iter begin fun (p, status) ->
      finish p status;
      num_processed := !num_processed + List.length p.batch
    end (List.rev ended);
    if ended = [] then Unix.sleepf poll_interval
  in
  Fun.protect ~finally:begin fun () ->
    List.iter abort !running;
    Common_args.clear_progress ()
  end @@ fun () ->
  fill ();
  report_progress ();
  while !running <> [] do
    let num_processed' = !num_processed in
    step ();
    fill ();
    if !num_processed <> num_processed' then report_progress ()
  done

let run ~jobs ?batch_size ~progress ~command ~args ~sequential files =
  let jobs = max 1 jobs and num_files = List.length files in
  let batch_size = match batch_size with
    | Some n -> max 1 n
    | None -> max 1 @@ min max_batch_size ((num_files + jobs - 1) / jobs)
  in
  if jobs > 1 && not supported then
    Pretty.error "@[<hov>Parallel@ processes@ are@ not@ supported@ by@ this@ \
                  build:@ files@ are@ processed@ in@ turn@]@.";
  if jobs < 2 || num_files <= batch_size || not supported then
    sequential files
  else
    run_batches ~jobs ~progress (batches batch_size files)
      ~argv:(Sys.executable_name :: command @ args @ ["--"])
