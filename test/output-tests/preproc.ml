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

open Format
open Ez_file
open FileString.OP
open Testsuite_utils

let preprocess_file ~source_format ~config filename =
  Cobol_common.Diagnostics.show_n_forget ~min_level:Error @@
  Cobol_preproc.Preprocess.preprocess_file filename
    ~options:Cobol_preproc.Options.{ source_format; config;
                                     verbose = false; libpath = [] }
    ~ppf:std_formatter

let () =
  (* Print one token per line so we can diff outputs more easily. *)
  Pretty.pp_set_margin std_formatter 3;
  let config = from_dialect Cobol_config.Dialect.mf_strict in
  deep_iter mf_root ~glob:"*.[cC][bB][lL]"
    ~f:begin fun path ->
      printf "@[<1>Pre-processing `%s':@\n" @@ mf_testsuite // path;
      preprocess_file ~source_format:(Cobol_config.Types.SF SFFixed) ~config
        (mf_root // path);
      printf "@]@\nDone.@."
    end;
  let config = Cobol_config.Config.default in
  deep_iter ibm_root ~glob:"*.cbl"
    ~f:begin fun path ->
      printf "@[<1>Pre-processing `%s':@\n" @@ ibm_testsuite // path;
      preprocess_file ~config ~source_format:(Cobol_config.Types.SF SFFree)
        (ibm_root // path);
      printf "@]@\nDone.@."
    end;
;;
