(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Batches of COBOL files analyzed by `superbol cache analyze' processes, that
    fill the caches of the language server. *)

open Vscode

let plural n one many = if n = 1 then one else many

let files_per_process_setting = "workspaceAnalysis.filesPerProcess"
let max_processes_setting = "workspaceAnalysis.maxProcesses"

let max_processes () =
  Superbol_config.positive_int max_processes_setting
    ~default:(max 1 (Array.length (Node.Os.cpus ()) - 1))

type job =
  {
    files: Uri.t list;
    project_root: string;
    client_settings: string;           (* `superbol.cobol' section, as JSON *)
  }

(* The server asks for the `superbol.cobol' section with the root directory of
   each project as scope, which resolves to the folder that holds it. *)
let client_settings_for folder =
  Jsonoo.stringify @@ Jsonoo.t_of_js @@
  Superbol_config.any ~scope:(`WorkspaceFolder folder) "cobol"

let rec chunks n = function
  | [] -> []
  | l ->
      let rec take i acc = function
        | x :: rest when i < n -> take (succ i) (x :: acc) rest
        | rest -> List.rev acc, rest
      in
      let chunk, rest = take 0 [] l in
      chunk :: chunks n rest

(* Files are sorted by path, so that each job sees as few projects as
   possible. *)
let make uris =
  let per_folder = Hashtbl.create 1 in
  List.iter begin fun uri ->
    match Workspace.getWorkspaceFolder ~uri with
    | None -> ()                               (* not found by `findFiles' *)
    | Some folder ->
        let key = Uri.toString (WorkspaceFolder.uri folder) () in
        let folder, uris =
          Option.value (Hashtbl.find_opt per_folder key) ~default:(folder, [])
        in
        Hashtbl.replace per_folder key (folder, uri :: uris)
  end uris;
  let files_per_process =
    Superbol_config.positive_int files_per_process_setting ~default:25 in
  Hashtbl.fold begin fun _ (folder, uris) jobs ->
    let client_settings = client_settings_for folder
    and project_root = Uri.fsPath (WorkspaceFolder.uri folder) in
    let uris =
      List.sort (fun a b -> String.compare (Uri.fsPath a) (Uri.fsPath b)) uris
    in
    List.map (fun files -> { files; project_root; client_settings })
      (chunks files_per_process uris) @ jobs
  end per_folder []

(* Keeps the end of what the process prints on its error output, for the log in
   case it fails; diagnostics are reported by the server afterwards. *)
let stderr_tail_length = 2048

(* Relative names of dialects, or of their configuration files, are looked up
   from the working directory first.  The language client starts the server in
   the first workspace folder (see `_getServerWorkingDir' in
   `vscode-languageclient/lib/node/main.js'), so jobs are started there too. *)
let server_working_directory () =
  match Workspace.workspaceFolders () with
  | folder :: _ when Uri.scheme (WorkspaceFolder.uri folder) = "file" ->
      Some (Uri.fsPath (WorkspaceFolder.uri folder))
  | _ ->
      None

let run_one (cli: Superbol_languageclient.cli_invocation) ~token job =
  Promise.make @@ fun ~resolve ~reject:_ ->
  let args =
    cli.args_prefix @ ["cache"; "analyze"] @ cli.cli_storage_args @
    (* The server puts every file of a workspace folder in the project of that
       folder, even if it has no `superbol.toml'. *)
    ["--project-root"; job.project_root;
     "--client-settings"; job.client_settings;
     (* Errors in COBOL files are reported by the server: any non-zero status
        then means some file could not be analyzed, and only failures to analyze
        a file are output. *)
     "--no-error-status"; "--no-diagnostics"] @
    List.map Uri.fsPath job.files
  in
  let settled = ref false in
  let settle ok = if not !settled then (settled := true; resolve ok) in
  let tail = ref "" in
  let keep_tail ~chunk =
    let s = match chunk with
      | `String s -> s
      | `Buffer b -> Node.Buffer.toString b
    in
    let s = !tail ^ s in
    let len = String.length s in
    tail := if len <= stderr_tail_length then s
      else String.sub s (len - stderr_tail_length) stderr_tail_length
  in
  let fail fmt =
    Printf.ksprintf begin fun msg ->
      Superbol_printer.log_error "SuperBOL: analysis of %u file%s failed: %s"
        (List.length job.files) (plural (List.length job.files) "" "s") msg;
      settle false
    end fmt
  in
  match
    Node.ChildProcess.spawn_process cli.program (Array.of_list args) ()
      ~options:(Node.ChildProcess.Options.create ~env:cli.cli_env
                  ?cwd:(server_working_directory ()) ())
  with
  | exception e ->
      fail "%s" (Printexc.to_string e)
  | process ->
      let cancellation =
        CancellationToken.onCancellationRequested token ()
          ~listener:(fun _ -> Node.ChildProcess.kill process ())
      in
      (* Output is consumed, so the process never blocks on a full pipe. *)
      Node.Stream.Readable.on (Node.ChildProcess.get_stdout process)
        (`Data (fun ~chunk:_ -> ()));
      Node.Stream.Readable.on (Node.ChildProcess.get_stderr process)
        (`Data keep_tail);
      Node.ChildProcess.on process @@ `Error begin fun ~err ->
        Disposable.dispose cancellation;
        fail "%s" (Node.JsError.message err)
      end;
      Node.ChildProcess.on process @@ `Close begin fun ~code ?signal:_ () ->
        Disposable.dispose cancellation;
        if code = 0 || CancellationToken.isCancellationRequested token
        then settle (code = 0)
        else fail "exit code %d\n%s" code !tail
      end

(* At most [max_processes ()] jobs run at once.  [on_completion] is called as
   soon as each job is over, whether it succeeded or not; no job is started once
   [token] is cancelled. *)
let run cli ~token ?(on_start = fun ~processes:_ -> ()) ~on_completion jobs =
  let open Promise.Syntax in
  let queue = Queue.of_seq @@ List.to_seq jobs in
  let rec worker () =
    if CancellationToken.isCancellationRequested token || Queue.is_empty queue
    then Promise.return ()
    else
      let job = Queue.pop queue in
      let* ok = run_one cli ~token job in
      on_completion job ~ok;
      worker ()
  in
  let processes = min (max_processes ()) (Queue.length queue) in
  on_start ~processes;
  let+ _ = Promise.all (Array.init processes (fun _ -> worker ())) in
  ()
