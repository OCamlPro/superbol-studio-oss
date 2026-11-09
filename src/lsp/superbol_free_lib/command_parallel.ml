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

(** Running any sub-command on batches of files, in parallel processes. *)

open Ezcmd.V2
open EZCMD.TYPES

let name = "parallel"

(* [split_at_separator words] is the words before the first [--] in [words],
   along with those after it, if any. *)
let split_at_separator words =
  let rec split acc = function
    | "--" :: rest -> List.rev acc, Some rest
    | w :: rest -> split (w :: acc) rest
    | [] -> List.rev acc, None
  in
  split [] words

(* Non-empty lines of [ic], without any trailing carriage return. *)
let read_lines ic =
  let rec read acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line ->
        let len = String.length line in
        let line =
          if len > 0 && line.[len - 1] = '\r'
          then String.sub line 0 (len - 1) else line
        in
        read (if line = "" then acc else line :: acc)
  in
  read []

(* Whether [command] starts with the name of one of [subcommands]
   (e.g. [cache analyze]). *)
let names_subcommand ~subcommands command =
  List.exists begin fun { sub_name; _ } ->
    let rec is_prefix = function
      | [], _ -> true
      | w :: name, w' :: command -> w = w' && is_prefix (name, command)
      | _ :: _, [] -> false
    in
    is_prefix (String.split_on_char ' ' sub_name, command)
  end subcommands

(** [make ~subcommands ~run_in_process] is the sub-command, that runs any of
    [subcommands]; [run_in_process args] runs this program with the arguments
    [args] (that do not include the name of the program) within the current
    process, and may not return. *)
let make ~subcommands ~run_in_process =
  let parallel, parallel_args = Parallel_batches.args () in
  let progress, progress_arg = Common_args.progress_arg () in
  let words = ref [] in
  EZCMD.sub name
    (fun () ->
       let command, files = split_at_separator !words in
       begin match command with
         | [] -> Cobol_common.fatal "Missing sub-command"
         | w :: _ when w = name -> Cobol_common.fatal "Nested `%s'" name
         | c when not (names_subcommand ~subcommands c) ->
             Cobol_common.fatal "Unknown sub-command: %s" (String.concat " " c)
         | _ -> ()
       end;
       let files = match files with
         | Some files -> files
         | None -> read_lines stdin
       in
       let jobs, batch_size = parallel () in
       if files <> [] then
         Parallel_batches.run ~jobs ?batch_size ~progress:(progress ())
           ~command ~args:[] files
           ~sequential:(fun files -> run_in_process (command @ "--" :: files)))
    ~args:(parallel_args @ progress_arg @
           [ [], Arg.Anons (fun l -> words := l),
             EZCMD.info ~docv:"SUB-COMMAND"
               "Sub-command to run, with its options, followed by $(b,--) and \
                the files to give it" ])
    ~doc:"Run a sub-command on batches of files, in parallel processes"
    ~man:[ `S "SYNOPSIS";
           `P "$(b,superbol parallel) [$(b,-j) N] [$(b,-n) K] $(b,--) \
               SUB-COMMAND [OPTION]... [$(b,--) FILE...]";
           `S "DESCRIPTION";
           `Blocks [ `P "Split the files into batches (see $(b,--batch-size)), \
                         and run $(b,superbol) SUB-COMMAND with its OPTIONs on \
                         each batch, in a process of its own, with up to N \
                         processes running at once (see $(b,--jobs)).  This is \
                         akin to $(b,xargs -P N -n K superbol) SUB-COMMAND \
                         [OPTION]..., except that outputs of processes never \
                         interleave, and that the exit status is computed over \
                         all of them, as if SUB-COMMAND had been run on every \
                         file at once.";
                     `P "The first $(b,--) ends the options of this command; \
                         files follow the second one.  Without a second \
                         $(b,--), files are read from the standard input, one \
                         per line.";
                     `P "The output of each process is shown as a whole when \
                         it ends, so outputs follow the order in which batches \
                         end rather than that of the given files.  Outputs are \
                         colored as those of this command, and laid out with \
                         the same margins.";
                     `P "With a single job, or in builds of this program into \
                         JavaScript, SUB-COMMAND is run on every file at once, \
                         within the same process." ];
           `S "EXAMPLES";
           `Pre "superbol parallel -j 8 -- cache analyze --stdout -- *.cbl\n\
                 find . -name '*.cbl' | superbol parallel -j 8 -- check syntax"
         ]
