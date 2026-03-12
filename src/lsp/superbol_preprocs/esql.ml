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

type Cobol_common.Exec_block.TYPES.exec_block +=
  | Esql_exec_block of Sql_ast.esql_instruction

let () =
  Cobol_common.Exec_block.register_exec_block_type
    ~name:"ESQL-EXEC-BLOCK"
    ~compare:begin fun a b -> match a, b with
      | Esql_exec_block a, Esql_exec_block b ->
          Some (Sql_ast.compare a b)
      | _ ->
          None
    end
    ~pp:begin function
      | Esql_exec_block a ->
          Some (Pretty.delayed "%a" Sql_ast.Printer.pp a)
      | _ ->
          None
    end

let scanner =
  Cobol_parser.Options.Stateless_exec_scanner (fun text ->
    Esql_exec_block (Sql_parser.parse text) , []
  )

let fold_exec_block': Cobol_typeck.Outputs.fold_exec_block'
  = fun ~register_name exec_block acc ->
    match exec_block.payload with
    | Esql_exec_block esql ->
      let cob_var_extractor_folder = object
        inherit [Sql_ast.cobolVarId list] Sql_ast.Visitor.folder
        method! fold_cobol_var_id cob_var acc =
          Cobol_common.Visitor.skip (cob_var::acc)
      end in
      let cobol_vars =
        Sql_ast.Visitor.fold_esql_instruction cob_var_extractor_folder
          esql []
      in
      List.fold_left begin fun acc cobol_var_id ->
        register_name cobol_var_id acc
      end acc cobol_vars
    | _ -> acc


