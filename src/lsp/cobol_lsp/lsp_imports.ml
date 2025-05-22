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

(** Definitions of module aliases and helper functors *)

module CUs = Cobol_unit.Collections.SET
module CUMap = Cobol_unit.Collections.MAP
module URIMap = struct
  include Map.Make (Lsp.Uri)

  let add_to_list x data m =
      let add = function None -> Some [data] | Some l -> Some (data :: l) in
      update x add m
end
