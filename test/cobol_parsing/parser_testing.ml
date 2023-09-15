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

let show_parsed_tokens
    ?(verbose = false)
    ?(source_format = Cobol_config.(SF SFFixed))
    prog =
  let { parsed_output = WithTokens (_, tokens, _log); _ } =
    Cobol_parser.parse_with_tokens ~verbose ~source_format
      ~recovery:(EnableRecovery { silence_benign_recoveries = false })
      ~libpath:[] @@
    Cobol_preproc.String { filename = "prog.cob"; contents = prog }
  in
  Cobol_parser.INTERNAL.pp_tokens Fmt.stdout (Lazy.force tokens)
