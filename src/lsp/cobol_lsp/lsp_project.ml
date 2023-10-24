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

open Cobol_common.Diagnostics.TYPES

module DIAGS = Cobol_common.Diagnostics
module TYPES = struct
  include Superbol_project.Config.TYPES
  include Superbol_project.TYPES
end

include TYPES
type t = project

module SET = Superbol_project.SET
module MAP = Superbol_project.MAP

(* --- *)

let rootdir = Superbol_project.rootdir
let string_of_rootdir = Superbol_project.string_of_rootdir

let rootdir_for ~uri ~layout =
  Superbol_project.rootdir_for ~filename:(Lsp.Uri.to_path uri) ~layout

let show_n_forget_diagnostics ?(force = false) { result = project; diags } =
  if force || diags <> DIAGS.Set.none then
    Lsp_diagnostics.publish @@
    Lsp_diagnostics.translate diags
      ~rootdir:(Superbol_project.string_of_rootdir project.rootdir)
      ~uri:(`Main (Lsp.Uri.of_path project.config_filename));
  project

let for_ ~rootdir ~layout =
  show_n_forget_diagnostics @@
  Superbol_project.for_ ~rootdir ~layout

let in_existing_dir dir ~layout =
  show_n_forget_diagnostics ~force:true @@
  Superbol_project.in_existing_dir dir ~layout

let libpath_for ~uri project =
  Superbol_project.libpath_for ~filename:(Lsp.Uri.to_path uri) project

let detect_copybook ~uri project =
  Superbol_project.detect_copybook ~filename:(Lsp.Uri.to_path uri) project

let relative_path_for ~uri project =
  Superbol_project.relative_path_for ~filename:(Lsp.Uri.to_path uri) project

let absolute_path_for =
  Superbol_project.absolute_path_for

(** Caching *)

type cached = Superbol_project.cached

let to_cache project =
  Superbol_project.to_cache project

let of_cache ~rootdir ~layout cached =
  show_n_forget_diagnostics @@
  Superbol_project.of_cache ~rootdir ~layout cached
