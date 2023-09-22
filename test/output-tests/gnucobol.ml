(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open Autofonce_lib
open Autofonce_config
open Autofonce_core.Types

let () =
  (* Disable backtrace recording so `OCAMLRUNPARAM=b` has no effect on the
     output of tests that fail expectedly. *)
  Stdlib.Printexc.record_backtrace false

(* Preliminary utilities *)

module STR = struct
  let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try ignore @@ Str.search_forward re s1 0; true
    with _ -> false
end

let srcdir = try Unix.getenv "DUNE_SOURCEROOT" with Not_found -> "."

let target =
  let target = ref None in
  Arg.parse [] (fun f -> target := Some f) "gnucobol.exe <target-basename.at>";
  !target

(** [pp_relloc ppf filename] prints [filename] relative to [srcdir] if the
    latter is a directory (prefix) of [filename].  Otherwise, prints [filename]
    as a whole. *)
let pp_relloc =
  let srcdir_prefix = srcdir ^ Ez_file.FileOS.dir_separator_string in
  fun ppf s ->
    match EzString.chop_prefix ~prefix:srcdir_prefix s with
    | Some s -> Fmt.string ppf s
    | None -> Fmt.string ppf s

let make_n_enter_rundir () =
  Superbol_testutils.Tempdir.make_n_enter "superbol-gnucobol-tests"

(* Read the testsuite. *)

let _pconf, _tconf, testsuite =
  let toml = Filename.concat srcdir ".autofonce" in
  let contents = EzFile.read_file toml in
  let project_config =
    Project_config.from_string ~computed:false ~file:toml contents in
  Testsuite.read project_config (List.hd project_config.project_testsuites)

(* let init_test_filter () = *)
(*   let open Ezcmd.V2.EZCMD.TYPES in *)
(*   begin match Filter.args with *)
(*     | (x, Arg.String f, _) :: _ -> *)
(*         Fmt.epr "%a@." Fmt.(list string) x; *)
(*         f "move" *)
(*     | _ -> failwith "unexpected non-String argument descriptor" *)
(*   end; *)
(*   Filter.select_tests (fun _ -> ()) testsuite *)

let targetted_test ({ test_loc = { file; _ }; _ }: test) =
  match target with
  | Some t -> t = Filename.basename file
  | _ -> true

(** Extact AT_CHECK actions that expect a successful return code.*)
let extract_successful_test_actions ({ test_actions; _ }: test) =
  List.to_seq test_actions |>
  Seq.filter_map begin function
    | AT_CHECK ({ check_retcode = Some 0; check_test; _ } as check) ->
        Some (check, check_test)
    | _ ->
        None
  end

(** Extract AT_DATA entries for which we expect a successful check to be run. *)
let extract_data_file_of_check_command: check * test -> _ = function
  | ({ check_command; _ } as check),
    ({ test_actions; _ } as test) ->
      List.to_seq test_actions |>
      Seq.filter_map begin function
        | AT_DATA { file; content } when STR.contains check_command file ->
            Some (file, content, test, check)
        | _ ->
            None
      end

(* We want to run the parser on selected data files only once.  To this end, we
   sort the tests based on locations and AT_DATA's filename. *)
let compare_data_files
    (fn1, _, { test_loc = { file = f1; line = l1; _ }; _ }, _)
    (fn2, _, { test_loc = { file = f2; line = l2; _ }; _ }, _) =
  let c = String.compare f1 f2 in
  if c <> 0 then c else
    let c = Int.compare l1 l2 in
    if c <> 0 then c else
      String.compare fn1 fn2

let filename_for_loc test_filename ({ file; line; _ }: location) =
  Pretty.to_string "%s-%u-%s" (Filename.basename file) line test_filename

let setup_input ~filename contents =
  let oc = open_out filename in
  output_string oc contents;
  close_out oc;
  Cobol_preproc.String { contents; filename }

let delete_file ~filename =
  Ez_file.FileString.remove filename

let free_format_regexp = Str.regexp {re|\(^-free\|[ \t]-free\b\)|re}
let do_check_parse (test_filename, contents, _, { check_loc;
                                                  check_command; _ }) =
  let filename = filename_for_loc test_filename check_loc in
  let terminate result_fmt =
    delete_file ~filename;
    Pretty.error (result_fmt ^^ "@.")
  in
  (* Note here we use the location of an AT_CHECK, as the location of the
     corresponding AT_DATA does not seem to be available at the moment. *)
  let loc = Autofonce_m4.M4Printer.string_of_location check_loc in
  Pretty.error "Considering: %a... " pp_relloc loc;
  Pretty.out   "Considering: %a@\n" pp_relloc loc;
  let source_format =                          (* hackish detection of format *)
    if EzString.ends_with ~suffix:"free.cob" filename ||
       try ignore (Str.search_forward free_format_regexp check_command 0); true
       with Not_found -> false
    then Cobol_config.(SF SFFree)
    else Cobol_config.(SF SFFixed)
  in
  try
    let input = setup_input ~filename contents in
    match Cobol_parser.parse_simple ~source_format ~libpath:[] input with
    | { parsed_diags; parsed_output = Only Some _; _ } ->
        Cobol_common.show_diagnostics ~ppf:Fmt.stdout parsed_diags;
        terminate "ok"
    | { parsed_diags; _ } ->
        Cobol_common.show_diagnostics ~ppf:Fmt.stdout parsed_diags;
        terminate "ok (with errors)"
    | exception e ->
        Pretty.out "Failure (%s)@\n%s@\n" (Printexc.to_string e) contents;
        Printexc.print_backtrace Stdlib.stdout;
        terminate "fail"
  with e ->
    Pretty.error "skipped (fatal)@.";
    Pretty.out   "Fatal: %s@\n" (Printexc.to_string e);
    Printexc.print_backtrace Stdlib.stdout

(* Main entry point *)

let () =
  Pretty.pp_set_margin Format.std_formatter 120;   (* fix a margin for stdout *)
  let _rundir = make_n_enter_rundir () in
  (* init_test_filter (); *)
  List.to_seq testsuite.suite_tests |>
  Seq.filter targetted_test |>
  Seq.flat_map extract_successful_test_actions |>
  Seq.flat_map extract_data_file_of_check_command |>
  List.of_seq |>
  List.sort_uniq compare_data_files |>
  List.iter do_check_parse
