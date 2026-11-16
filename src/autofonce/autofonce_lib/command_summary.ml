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
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1

type status =
    GOOD
  | FAIL
  | KOOK
  | SKIP

type test = {
  num : int ;
  mutable status : status ;
  name : string ;
  location : string ;
}

type summary = {
  filename : string ;
  mutable tests : test IntMap.t ;
}

let parse_line line =
  let num, line = EzString.cut_at line ' ' in
  let status, line = EzString.cut_at line ' ' in
  let location, name = EzString.cut_at line ')' in
  let location = String.sub location 3 (String.length location - 3) in
  let name = String.trim name in
  let status = match status with
    | "GOOD" -> GOOD
    | "FAIL" -> FAIL
    | "KOOK" -> KOOK
    | "SKIP" -> SKIP
    | _ ->
        Printf.eprintf "Unknown status [%s]\n%!" status;
        GOOD
  in
  {
    num = int_of_string num ;
    status ;
    name ;
    location ;
  }

let action _p _suites summaries =

  let all_tests = ref IntMap.empty in
  let summaries =
    List.map (fun filename ->
        let s = {
          filename ;
          tests = IntMap.empty ;
        } in
        let lines = EzFile.read_lines filename in
        Array.iter (fun line ->
            let test = parse_line line in
            s.tests <- IntMap.add test.num test s.tests;
            let shared = match IntMap.find test.num !all_tests with
              | shared -> shared
              | exception _ ->
                  let shared = { test with status = GOOD } in
                  all_tests := IntMap.add test.num test !all_tests;
                  shared
            in
            begin
              match shared.status, test.status with
              | GOOD, GOOD -> ()
              | _ -> shared.status <- test.status
            end;
            ())
          lines;
        s
      ) summaries
  in
  let numtests =
    match IntMap.max_binding !all_tests with
    | num, _ -> num
    | exception _ -> 0
  in

  let find_test num tests =
    try IntMap.find num tests with
    | _ -> {
        num ;
        name = "";
        location = "";
        status = SKIP;
      }
  in

  let id name = Printf.sprintf " id=\"%s\"" name in
  let id_table = id "test_table" in
  let id_td_num = id "test_td_num" in
  let id_td_name = id "test_td_name" in
  let id_td_location = id "test_td_location" in
  let id_td_GOOD = id "test_td_good" in
  let id_td_FAIL = id "test_td_fail" in
  let id_td_KOOK = id "test_td_kook" in
  let id_td_SKIP = id "test_td_skip" in

  let print_table b filter =
    Printf.bprintf b "<table%s>\n" id_table;
    Printf.bprintf b "<thead><tr>\n";
    Printf.bprintf b "  <td>Num</td>\n";
    List.iter (fun s ->
        Printf.bprintf b "  <td>%s</td>\n" s.filename;
      ) summaries;
    Printf.bprintf b "  <td>Name</td>\n";
    Printf.bprintf b "  <td>Location</td>\n";
    Printf.bprintf b "</tr></thead>\n";
    Printf.bprintf b "<tbody>\n";
    for num = 1 to numtests do
      let t = find_test num !all_tests in
      if filter t then begin
        Printf.bprintf b "<tr>\n";
        Printf.bprintf b "  <td%s>%d</td>\n" id_td_num t.num;
        List.iter (fun s ->
            let t = find_test num s.tests in
            Printf.bprintf b "  <td%s>%s</td>\n"
              (match t.status with
               | GOOD -> id_td_GOOD
               | FAIL -> id_td_FAIL
               | KOOK -> id_td_KOOK
               | SKIP -> id_td_SKIP
              )
              (match t.status with
               | GOOD -> "GOOD"
               | FAIL -> "FAIL"
               | KOOK -> "KOOK"
               | SKIP -> "SKIP"
              );
          ) summaries;
        Printf.bprintf b "  <td%s>%s</td>\n" id_td_name t.name;
        Printf.bprintf b "  <td%s>%s</td>\n" id_td_location t.location;
        Printf.bprintf b "</tr>\n";
      end
    done;
    Printf.bprintf b "</tbody>\n";
    Printf.bprintf b "</table>\n";
  in

  let b = Buffer.create 10000 in
  Printf.bprintf b "<h2>Not GOOD<h2>\n";
  print_table b (fun t -> t.status <> GOOD);
  Printf.bprintf b "<h2>All GOOD<h2>\n";
  print_table b (fun t -> t.status = GOOD);
  EzFile.write_file "summary.html" (Buffer.contents b)

let cmd =
  let all_files = ref [] in
  let testsuite_args, get_testsuite_args = Testsuite.args () in
  let args =
    testsuite_args @
    [

      [], Arg.Anons (fun files ->
          all_files := !all_files @ files
        ),
      EZCMD.info "List of files to analyze" ;

    ]
  in
  EZCMD.sub
    "summary"
    (fun () ->
       let p, suites = Testsuite.find ( get_testsuite_args () ) in
       action p suites !all_files
    )
    ~args
    ~doc: "Merge results summaries into a table"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Use this command to create an HTML table with results summaries" ;
      ];
    ]
