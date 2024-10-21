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

module StrMap = EzCompat.StringMap
module LSP = Cobol_lsp.INTERNAL

type test_project =
  {
    projdir: string;
    end_with_postproc: string -> unit;
  }

val make_lsp_project
  : ?toml:string
  -> unit
  -> test_project * LSP.Types.registry
val add_cobol_doc
  : LSP.Types.registry -> projdir:string -> string -> string
  -> LSP.Types.registry * Lsp.Types.TextDocumentIdentifier.t

type positions =
  {
    pos_anonymous: Lsp.Types.Position.t list;
    pos_map: Lsp.Types.Position.t EzCompat.StringMap.t;
  }

val extract_position_markers: string -> string * positions

class srcloc_resuscitator_cache: object
  method of_: location:Lsp.Types.Location.t -> Cobol_common.Srcloc.srcloc
  method pp: Lsp.Types.Location.t Pretty.printer
  method print: Lsp.Types.Location.t -> unit
  method print_range_for: uri:Lsp.Uri.t -> Lsp.Types.Range.t -> unit
  method print_optional_range_for: uri:Lsp.Uri.t -> Lsp.Types.Range.t option -> unit
end
