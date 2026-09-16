
(* This library defines 3 modules:

* Unix: a limited version of the Unix library, where non-portable calls have been
   replaced by the `unimplemented` type.
* EzUnix: a version of Unix working only a Unix systems, not Win32, with some additional
    functions
* EzWin32: a set of functions working only on Win32 systems


*)

module EzUnix = struct
  include Unix

  include (struct

    let uninterrupted_wait () =
      let rec iter () =
        try
          wait ()
        with MinUnix.Unix_error(MinUnix.EINTR, _, _) ->
          iter ()
      in
      iter ()

  end : sig

   val uninterrupted_wait : unit -> int * Unix.process_status

  end )
end

module Unix = struct

  include Unix

  type unimplemented

  let unimplemented = (Obj.magic () : unimplemented)

  let wait = unimplemented
  let fork = unimplemented
  let getppid = unimplemented
  let nice = unimplemented
  let fchmod = unimplemented
  let chown = unimplemented
  let fchown = unimplemented
  let umask = unimplemented
  let chroot = unimplemented
  let mkfifo = unimplemented
  let sigprocmask = unimplemented
  let sigpending = unimplemented
  let sigsuspend = unimplemented
  let pause = unimplemented
  let alarm = unimplemented
  let getitimer = unimplemented
  let setitimer = unimplemented
  let setuid = unimplemented
  let setgit = unimplemented
  let setgroups = unimplemented
  let initgroups = unimplemented
  let establish_server = unimplemented
  let tcgetattr = unimplemented
  let tcsetattr = unimplemented
  let tcsendbreak = unimplemented
  let tcdrain = unimplemented
  let tcflush = unimplemented
  let tcflow = unimplemented
  let setsid = unimplemented

  include ( struct

    type os_type = WINDOWS | CYGWIN | UNIX

    let os_type =
      match String.lowercase_ascii Sys.os_type with
        "win32" -> WINDOWS
      | "cygwin" -> CYGWIN
      | "unix" -> UNIX
      | _ -> assert false

    let rec uninterrupted_waitpid pid =
      match waitpid [] pid with
      | (_, status) ->
          begin
            match status with
            | MinUnix.WEXITED n -> n
            (* Is-it the expected behavior ? *)
            | MinUnix.WSIGNALED _ -> uninterrupted_waitpid pid
            | MinUnix.WSTOPPED _ -> uninterrupted_waitpid pid
          end
      | exception MinUnix.Unix_error (MinUnix.EINTR, _, _) ->
          uninterrupted_waitpid pid

  end : sig
              type os_type = WINDOWS | CYGWIN | UNIX
              val os_type : os_type
              val uninterrupted_waitpid : int -> int
            end )


end


module EzWin32 : sig

  open Unix

  val command : string array -> int
  val simulate_exec : string array -> 'a

  val waitpids : int array -> int * Unix.process_status

  type fileinfo = {
    dwFileAttributes : int;
    ftCreationTime : float; (* in Unix seconds *)
    ftLastAccessTime : float; (* in Unix seconds *)
    ftLastWriteTime : float; (* in Unix seconds *)
    dwVolumeSerialNumber : int;
    nFileSize : int64;
    nNumberOfLinks : int;
    nFileIndex : int64;
  }

  external getFileInformationByHandle : Unix.file_descr -> fileinfo
    = "onlyWin32_getFileInformationByHandle_ml"

  external getFileInformationByName : string -> fileinfo
    = "onlyWin32_getFileInformationByName_ml"

  (** {6 High-level process and redirection management} *)

  (* All the following functions differ from OCaml's Unix library by
     improving on the quoting of commands *)

  val create_process :
    string -> string array -> file_descr -> file_descr -> file_descr -> int
  (** [create_process prog args new_stdin new_stdout new_stderr]
      forks a new process that executes the program
      in file [prog], with arguments [args]. The pid of the new
      process is returned immediately; the new process executes
      concurrently with the current process.
      The standard input and outputs of the new process are connected
      to the descriptors [new_stdin], [new_stdout] and [new_stderr].
      Passing e.g. [stdout] for [new_stdout] prevents the redirection
      and causes the new process to have the same standard output
      as the current process.
      The executable file [prog] is searched in the path.
      The new process has the same environment as the current process. *)

  val create_process_env :
    string -> string array -> string array -> file_descr -> file_descr ->
    file_descr -> int
  (** [create_process_env prog args env new_stdin new_stdout new_stderr]
      works as {!Unix.create_process}, except that the extra argument
      [env] specifies the environment passed to the program. *)


  val open_process_in : string -> in_channel
  (** High-level pipe and process management. This function
      runs the given command in parallel with the program.
      The standard output of the command is redirected to a pipe,
      which can be read via the returned input channel.
      The command is interpreted by the shell [/bin/sh] (cf. [system]). *)

  val open_process_out : string -> out_channel
  (** Same as {!Unix.open_process_in}, but redirect the standard input of
      the command to a pipe.  Data written to the returned output channel
      is sent to the standard input of the command.
      Warning: writes on output channels are buffered, hence be careful
      to call {!Pervasives.flush} at the right times to ensure
      correct synchronization. *)

  val open_process : string -> in_channel * out_channel
  (** Same as {!Unix.open_process_out}, but redirects both the standard input
      and standard output of the command to pipes connected to the two
      returned channels.  The input channel is connected to the output
      of the command, and the output channel to the input of the command. *)

  val open_process_full :
    string -> string array -> in_channel * out_channel * in_channel
  (** Similar to {!Unix.open_process}, but the second argument specifies
      the environment passed to the command.  The result is a triple
      of channels connected respectively to the standard output, standard input,
      and standard error of the command. *)

  val close_process_in : in_channel -> process_status
  (** Close channels opened by {!Unix.open_process_in},
      wait for the associated command to terminate,
      and return its termination status. *)

  val close_process_out : out_channel -> process_status
  (** Close channels opened by {!Unix.open_process_out},
      wait for the associated command to terminate,
      and return its termination status. *)

  val close_process : in_channel * out_channel -> process_status
  (** Close channels opened by {!Unix.open_process},
      wait for the associated command to terminate,
      and return its termination status. *)

  val close_process_full :
    in_channel * out_channel * in_channel -> process_status
  (** Close channels opened by {!Unix.open_process_full},
      wait for the associated command to terminate,
      and return its termination status. *)

  (*
  type dir_handle
  (** The type of descriptors over opened directories. *)

  val opendir : string -> dir_handle
  (** Open a descriptor on a directory *)

  val readdir : dir_handle -> string
  (** Return the next entry in a directory.
      @raise End_of_file when the end of the directory has been reached. *)

  val rewinddir : dir_handle -> unit
  (** Reposition the descriptor to the beginning of the directory *)

  val closedir : dir_handle -> unit
  (** Close a directory descriptor. *)
*)

  val create_process_chdir :
    ?chdir:string -> ?env:string ->
    cmd:string -> string ->
    stdin:file_descr -> stdout:file_descr -> stderr:file_descr -> int

  type hroot =
    | HKEY_CLASSES_ROOT
    | HKEY_CURRENT_CONFIG
    | HKEY_CURRENT_USER
    | HKEY_LOCAL_MACHINE
    | HKEY_USERS

  type dwType =
    | REG_SZ
    | REG_EXPAND_SZ
    | REG_DWORD
    | REG_QWORD
    | REG_BINARY
    | REG_NONE

  type rkey_value = string array * string * dwType

  val string_of_rkey : rkey_value -> string
  val read_standard_config : rkey_value -> dwType * string

  val read_key : hroot -> rkey_value -> dwType * string
  val write_key : hroot -> rkey_value -> string -> unit
  val delete_key : hroot -> rkey_value -> bool

  val broadcast_setting_change : string -> unit

end = struct

  open Unix
  module MinUnix = Unix

  external waitpids : int -> int array -> int * MinUnix.process_status
    = "onlyWin32_waitpids_ml"
  let waitpids pids = waitpids (Array.length pids) pids

  external create_process_chdir : string -> string -> string option ->
    file_descr -> file_descr -> file_descr -> string option -> int
    = "onlyWin32_create_process_chdir"
      "onlyWin32_create_process_chdir_native"

  let create_process_chdir ?chdir ?env ~cmd cmdline ~stdin ~stdout ~stderr =
    create_process_chdir cmd cmdline env stdin stdout stderr chdir

  type fileinfo = {
    dwFileAttributes : int;
    ftCreationTime : float; (* in Unix seconds *)
    ftLastAccessTime : float; (* in Unix seconds *)
    ftLastWriteTime : float; (* in Unix seconds *)
    dwVolumeSerialNumber : int;
    nFileSize : int64;
    nNumberOfLinks : int;
    nFileIndex : int64;
  }

  external getFileInformationByHandle : MinUnix.file_descr -> fileinfo
    = "onlyWin32_getFileInformationByHandle_ml"

  external getFileInformationByName : string -> fileinfo
    = "onlyWin32_getFileInformationByName_ml"

  (* High-level process management (system, popen) *)

  (* Slightly improved version compared to OCaml's version *)
  let make_cmdline args =
    let maybe_quote f =
      if String.contains f ' ' || String.contains f '\"'
      then Filename.quote f
      else f in
    String.concat " " (List.map maybe_quote (Array.to_list args))

  let create_process prog args stdin stdout stderr =
    create_process_chdir prog ~cmd:(make_cmdline args) ~stdin ~stdout ~stderr

  let create_process_env prog args env stdin stdout stderr =
    create_process_chdir prog ~cmd:(make_cmdline args)
      ~env:(String.concat "\000" (Array.to_list env) ^ "\000")
      ~stdin ~stdout ~stderr

  type popen_process =
      Process of in_channel * out_channel
    | Process_in of in_channel
    | Process_out of out_channel
    | Process_full of in_channel * out_channel * in_channel

  let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

  let shell = lazy
    (
      try Sys.getenv "COMSPEC"
      with Not_found -> raise(Unix_error(ENOEXEC, "open_proc",
                                         "COMPSEC is not defined"))
    )

  let open_proc cmd optenv proc stdin stdout stderr =
    let shell = Lazy.force shell in
    let pid =
      create_process_chdir shell ~cmd:(shell ^ " /c " ^ cmd) ?env:optenv
        ~stdin ~stdout ~stderr in
    Hashtbl.add popen_processes proc pid

  let open_process_in cmd =
    let (in_read, in_write) = pipe() in
    set_close_on_exec in_read;
    let inchan = in_channel_of_descr in_read in
    open_proc cmd None (Process_in inchan) stdin in_write stderr;
    close in_write;
    inchan

  let open_process_out cmd =
    let (out_read, out_write) = pipe() in
    set_close_on_exec out_write;
    let outchan = out_channel_of_descr out_write in
    open_proc cmd None (Process_out outchan) out_read stdout stderr;
    close out_read;
    outchan

  let open_process cmd =
    let (in_read, in_write) = pipe() in
    let (out_read, out_write) = pipe() in
    set_close_on_exec in_read;
    set_close_on_exec out_write;
    let inchan = in_channel_of_descr in_read in
    let outchan = out_channel_of_descr out_write in
    open_proc cmd None (Process(inchan, outchan)) out_read in_write stderr;
    close out_read; close in_write;
    (inchan, outchan)

  let open_process_full cmd env =
    let (in_read, in_write) = pipe() in
    let (out_read, out_write) = pipe() in
    let (err_read, err_write) = pipe() in
    set_close_on_exec in_read;
    set_close_on_exec out_write;
    set_close_on_exec err_read;
    let inchan = in_channel_of_descr in_read in
    let outchan = out_channel_of_descr out_write in
    let errchan = in_channel_of_descr err_read in
    open_proc cmd (Some(String.concat "\000" (Array.to_list env) ^ "\000"))
      (Process_full(inchan, outchan, errchan))
      out_read in_write err_write;
    close out_read; close in_write; close err_write;
    (inchan, outchan, errchan)

  let find_proc_id fun_name proc =
    try
      let pid = Hashtbl.find popen_processes proc in
      Hashtbl.remove popen_processes proc;
      pid
    with Not_found ->
      raise(Unix_error(EBADF, fun_name, ""))

  let close_process_in inchan =
    let pid = find_proc_id "close_process_in" (Process_in inchan) in
    close_in inchan;
    snd(waitpid [] pid)

  let close_process_out outchan =
    let pid = find_proc_id "close_process_out" (Process_out outchan) in
    close_out outchan;
    snd(waitpid [] pid)

  let close_process (inchan, outchan) =
    let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
    close_in inchan; close_out outchan;
    snd(waitpid [] pid)

  let close_process_full (inchan, outchan, errchan) =
    let pid =
      find_proc_id "close_process_full"
        (Process_full(inchan, outchan, errchan)) in
    close_in inchan; close_out outchan; close_in errchan;
    snd(waitpid [] pid)

(*
  type dir_entry =
      Dir_empty
    | Dir_read of string
    | Dir_toread

  type dir_handle =
    { dirname: string; mutable handle: int; mutable entry_read: dir_entry }

  external findfirst : string -> string * int = "onlyWin32_findfirst"
  external findnext : int -> string= "onlyWin32_findnext"

  let opendir dirname =
    try
      let (first_entry, handle) = findfirst (Filename.concat dirname "*.*") in
      { dirname = dirname; handle = handle; entry_read = Dir_read first_entry }
    with End_of_file ->
      { dirname = dirname; handle = 0; entry_read = Dir_empty }

  let readdir d =
    match d.entry_read with
      Dir_empty -> raise End_of_file
    | Dir_read name -> d.entry_read <- Dir_toread; name
    | Dir_toread -> findnext d.handle

  external win_findclose : int -> unit = "onlyWin32_findclose"

  let closedir d =
    match d.entry_read with
      Dir_empty -> ()
    | _ -> win_findclose d.handle

  let rewinddir d =
    closedir d;
    try
      let (first_entry, handle) = findfirst (d.dirname ^ "\\*.*") in
      d.handle <- handle; d.entry_read <- Dir_read first_entry
    with End_of_file ->
      d.handle <- 0; d.entry_read <- Dir_empty
   *)

  let command argv =
    (*    Printf.fprintf stderr "exec %s\n%!" filename; *)
    let pid =
      create_process argv.(0) argv
        MinUnix.stdin MinUnix.stdout MinUnix.stderr
    in
    let status = uninterrupted_waitpid pid in
    (*    Printf.fprintf stderr "waitpid returned %d\n%!" status; *)
    status

  let simulate_exec argv =
    let status = command argv in
    exit status




  (* Win32 Registry Manipulation Functions *)

  type hroot =
    | HKEY_CLASSES_ROOT
    | HKEY_CURRENT_CONFIG
    | HKEY_CURRENT_USER
    | HKEY_LOCAL_MACHINE
    | HKEY_USERS

  type dwType =
    | REG_SZ
    | REG_EXPAND_SZ
    | REG_DWORD
    | REG_QWORD
    | REG_BINARY
    | REG_NONE

  type rkey_value = string array * string * dwType

  external read_registry_key :
    hroot ->
    string array ->
    string ->
    dwType ref ->
    (* string to read (result) *)  bytes ->
    (* size read *) int
    = "win32_read_registry_value_ml"

  external write_registry_key :
    hroot ->
    string array ->
    string ->
    dwType ->
    string -> unit
    = "win32_write_registry_value_ml"

  external delete_registry_key :
    hroot ->
    string array ->
    string -> bool
    = "win32_delete_registry_value_ml"


  let read_key hroot (s2, s3, _) =
    let len = 8192 in
    let s = Bytes.create len in
    let dwType = ref REG_SZ in
    let res_len = read_registry_key hroot s2 s3 dwType s in
    if res_len < 0 then raise Not_found;
    !dwType, Bytes.sub_string s 0 res_len

  let write_key hroot (s2,s3,dwType) s =
    write_registry_key hroot s2 s3 dwType s

  let delete_key hroot (s2,s3,_) =
    delete_registry_key hroot s2 s3

  let string_of_rkey (path, entry, _) =
    String.concat "\\" (Array.to_list path @ [entry])

  let read_standard_config key =
    try
      (*    if !verbose then
             Printf.fprintf stderr "Trying HKCU\\%s...\n%!"
             (string_of_reg_key key); *)
      read_key HKEY_CURRENT_USER key
    with _ ->
      (*    if !verbose then
             Printf.fprintf stderr "Trying HKCU\\%s...\n%!"
             (string_of_reg_key key); *)
      read_key HKEY_LOCAL_MACHINE key

  external broadcast_setting_change : string -> unit =
    "win32_broadcast_setting_change_ml"

end
