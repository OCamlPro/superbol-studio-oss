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

open EzCompat

open Ez_file.V1
open EzFile.OP
open Types

module Misc = Autofonce_misc.Misc

type args = {
  mutable arg_filter : bool ;

  mutable arg_failures : string option ;
  mutable arg_exec_after : int ;
  mutable arg_exec_before : int ;
  mutable arg_tests_ids : ( int * int ) list ;
  mutable arg_tests_keywords : string list ;
  mutable arg_tests_nokeywords : string list ;
  mutable arg_only_failed : bool ;
  mutable arg_all_keywords : bool ;
}

let args () =
  {
    arg_filter = false ;
    arg_failures = None ;
    arg_exec_after = 0 ;
    arg_exec_before = max_int ;
    arg_tests_ids = [] ;
    arg_tests_keywords = [] ;
    arg_tests_nokeywords = [] ;
    arg_only_failed = false ;
    arg_all_keywords = false ;
  }

let select_tests ~args ?state select_test suite =
  let ntests = suite.suite_ntests in
  let all_tests =
    args.arg_tests_ids = [] && args.arg_tests_keywords = []
  in
  let id_set =
    match args.arg_tests_ids with
    | [] -> Array.make (ntests+1) false
    | ids ->
        let t = Array.make (ntests+1) false in
        List.iter (fun (id1,id2) ->
            for i = (max id1 1) to (min id2 ntests) do
              t.(i) <- true
            done;
          ) ids;
        t
  in
  let keyword_set, nokeyword_set =
    let yes_set = ref StringSet.empty in
    let no_set = ref StringSet.empty in
    begin
      match args.arg_tests_keywords with
      | [] -> ()
      | ids ->
          List.iter (fun s ->
              let s = String.lowercase_ascii s in
              let len = String.length s in
              if len>0 && s.[0] = '-' then
                no_set := StringSet.add ( String.sub s 1 (len-1) )
                    !no_set
              else
                yes_set := StringSet.add s !yes_set
            ) ids
    end;
    begin
      match args.arg_tests_nokeywords with
      | [] -> ()
      | ids ->
          no_set := StringSet.union !no_set ( StringSet.of_list ids )
    end;
    !yes_set, !no_set
  in
  List.iter (fun t ->
      if t.test_id >= args.arg_exec_after
      && t.test_id <= args.arg_exec_before
      && (all_tests
          || id_set. (t.test_id)
          ||
          (if args.arg_all_keywords then
             StringSet.for_all
               (fun k ->  StringSet.mem k t.test_keywords_set) keyword_set
           else
             StringSet.exists (fun k ->  StringSet.mem k keyword_set) t.test_keywords_set)
         )
      && not (
          StringSet.exists (fun k -> StringSet.mem k nokeyword_set)
            t.test_keywords_set
        )
      then
        if args.arg_only_failed then begin
          (* only_failed option should only be available
                                   with state *)
          match state with
          | None ->
              Misc.error "Options --failed/--failure only works with 'run' or 'promote'"
          | Some state ->
              let test_dir = Runner_common.test_dir t in
              let test_dir = state.state_run_dir // test_dir in
              if Sys.file_exists test_dir then
                match args.arg_failures with
                | None ->
                    select_test t
                | Some failure ->
                    let reason =
                      let failure_exitcode = ref false in
                      let failure_stdout = ref false in
                      let failure_stderr = ref false in
                      let files = Sys.readdir test_dir in
                      Array.iter (fun file ->
                          if Filename.check_suffix file ".exit.expected" then
                            failure_exitcode := true
                          else
                          if Filename.check_suffix file ".out.expected" then
                            failure_stdout := true
                          else
                          if Filename.check_suffix file ".err.expected" then
                            failure_stderr := true
                        ) files ;
                      String.concat " " (
                        begin
                          if !failure_exitcode then
                            [ "exitcode" ]
                          else
                            []
                        end
                        @
                        begin
                          if !failure_stdout then
                            [ "stdout" ]
                          else
                            []
                        end
                        @
                        begin
                          if !failure_stderr then
                            [ "stderr" ]
                          else
                            []
                        end
                      )
                    in
                    if failure = reason then
                      select_test t
        end else
          select_test t
    )
    suite.suite_tests

open Ezcmd.V2
open EZCMD.TYPES


let args () =
  let args = args () in
  let get_args () = args in
  let set_id s =
    try
      let range =
        match EzString.split s '-' with
        | [id1 ; "" ] -> (int_of_string id1, max_int)
        | [id1 ; id2 ] -> (int_of_string id1, int_of_string id2)
        | [id] ->
            let id = int_of_string id in
            if id < 0 then (0,-id-1) else (id,id)
        | _ -> raise Exit
      in
      args.arg_tests_ids <- args.arg_tests_ids @ [ range ]
    with _ ->
      args.arg_tests_keywords <- args.arg_tests_keywords @ [s]
  in
  [

    [ "k"; "keywords" ], Arg.String (fun s ->
        args.arg_tests_keywords <- args.arg_tests_keywords @
                                   EzString.split_simplify s ' ';
        args.arg_filter <- true
      ),
    EZCMD.info ~docv:"KEYWORD" "Run only tests matching KEYWORD";

    [ "i"; "ids" ], Arg.String (fun ids ->
        List.iter set_id @@ EzString.split_simplify ids ' ' ;
        args.arg_filter <- true
      ),
    EZCMD.info ~docv:"ID" "Run only test ID";

    [ "ge" ; "after" ], Arg.Int (fun x ->
        args.arg_exec_after <- x;
        args.arg_filter <- true
        ;
      ),
    EZCMD.info ~docv:"ID" "Exec starting at test $(docv)";

    [ "le" ; "before" ], Arg.Int (fun x ->
        args.arg_exec_before <- x;
        args.arg_filter <- true
        ;
      ),
    EZCMD.info ~docv:"ID" "Exec ending at test $(docv)";

    [ "N"; "not" ], Arg.String (fun s ->
        args.arg_tests_nokeywords <- args.arg_tests_nokeywords @
                            EzString.split_simplify s ' ';
        args.arg_filter <- true

      ),
    EZCMD.info ~docv:"KEYWORD" "Skip tests matching KEYWORD";

    [ "failures" ], Arg.String (fun s ->
        args.arg_only_failed <- true ;
        args.arg_failures <- Some s;
        args.arg_filter <- true

      ),
    EZCMD.info ~docv:"REASON" "Run failed tests with given failure";

    [ "F"; "failed" ], Arg.Unit (fun () ->
        args.arg_only_failed <- true ;
        args.arg_filter <- true

      ),
    EZCMD.info "Run only previously failed tests (among selected tests)";

    [ "A" ; "match-all" ], Arg.Unit (fun () ->
        args.arg_all_keywords <- true),
    EZCMD.info "Run tests matching all keywords instead of only one";

    [], Arg.Anons (fun list ->
        match list with
        | [] -> ()
        | _ ->
            List.iter (fun s ->
                set_id s
              ) list ;
            args.arg_filter <- true  ;
      ),
    EZCMD.info ~docv:"ID" "Exec ending at test $(docv)";

  ], get_args
