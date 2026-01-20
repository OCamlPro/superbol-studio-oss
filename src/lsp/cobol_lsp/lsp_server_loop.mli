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

module TYPES: sig
  type extensions =
    {
      alternate_request_handers: Lsp_request.TYPES.alternate_handler list;
    }
end
include module type of TYPES
  with type extensions = TYPES.extensions

val default_extensions: extensions

val config
  : project_layout: Superbol_project.layout
  -> ?enable_caching: bool
  -> ?enable_client_configs: bool
  -> ?force_syntax_diagnostics: bool
  -> ?fallback_storage_directory: string
  -> unit
  -> Lsp_server.config

val run
  : config: Lsp_server.config
  -> extensions: extensions
  -> Lsp_server.exit_status
