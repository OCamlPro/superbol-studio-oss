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

let options_with_unreadable_libs () =
  let options = Preproc_testing.options () in
  let platform =
    { options.platform with
      find_lib = (fun ~lookup_config:_ ?fromfile:_ ?libname:_ -> function
          | `Alphanum w | `Word w -> Ok (w ^ ".cpy"));
      read_text_file = (fun file ->
          raise @@ Sys_error (file ^ ": Permission denied")) }
  in
  { options with platform }

let%expect_test "unreadable-copybook" =
  Cobol_preproc.Outputs.show_n_forget ~ppf:Fmt.stdout @@
  Cobol_preproc.preprocess_input ~options:(options_with_unreadable_libs ()) @@
  Cobol_preproc.Input.string ~filename:"prog.cob" {|
       PROGRAM-ID. prog.
       COPY UNREADABLE.
       STOP RUN.
|};
  [%expect {|
    PROGRAM-ID . prog . STOP RUN .
    prog.cob:3.7-3.23:
    >> Error: Unable to read library `UNREADABLE.cpy':
              UNREADABLE.cpy: Permission denied |}]

let%expect_test "line-directive-out-of-range" =
  Preproc_testing.preprocess ~source_format:Cobol_config.(SF SFFree) {|
#line 99999999999999999999999999 "other.cob"
PROGRAM-ID. prog.
|};
  [%expect {|
    PROGRAM-ID . prog .
    prog.cob:2.0-2.44:
    >> Error: Unexpected line number `99999999999999999999999999' (out of range) |}]
