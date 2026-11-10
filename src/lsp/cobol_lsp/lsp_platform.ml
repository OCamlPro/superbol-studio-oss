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

(** Note: fields of this record are further initialized when entering
    `Lsp_server_loop.run` *)
let record =
  (* A copy: its fields are set when the server starts, which must leave the
     innocuous platform as is. *)
  Cobol_common.Platform.{ innocuous with verbosity = innocuous.verbosity }
