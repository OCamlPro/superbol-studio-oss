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

type Cobol_common.Exec_block.TYPES.exec_block +=   (* TEMPORARY, for testing. *)
  | Generic_exec_block of Cobol_preproc.Text.t

let () =
  Cobol_common.Exec_block.register_exec_block_type
    ~name:"GENERIC-EXEC-BLOCK"
    ~compare:begin fun a b -> match a, b with
      | Generic_exec_block a, Generic_exec_block b ->
          Some ((* FIXME: text comparison *)Stdlib.compare a b)
      | _ ->
          None
    end
    ~pp:begin function
      | Generic_exec_block a ->
          Some (Pretty.delayed "%a" Cobol_preproc.Text.pp_text a)
      | _ ->
          None
    end

let scanner =
  Cobol_parser.Options.Stateless_exec_scanner begin fun text ->
    Generic_exec_block text, []
  end
