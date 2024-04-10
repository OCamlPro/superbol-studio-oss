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

let exec_scanners =
  Cobol_parser.Options.{
    exec_scanner_fallback = Generic.scanner;  (* for now; TODO: Call.scanner? *)
    exec_scanners = Cobol_preproc.Options.EXEC_MAP.empty;
  }
