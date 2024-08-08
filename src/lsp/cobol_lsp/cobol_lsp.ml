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

include Lsp_server_loop

(* --- *)

(** {1 Modules and functions exported for testing purposes}

    Signatures of modules below may change unexpectedly. *)

module INTERNAL = struct
  module Types = struct
    include Lsp_imports
    include Lsp_diagnostics.TYPES
    include Lsp_lookup.TYPES
    include Lsp_document.TYPES
    include Lsp_project.TYPES
    include Lsp_project_cache.TYPES
    include Lsp_server.TYPES
  end
  module Diagnostics = Lsp_diagnostics
  module Lookup = Lsp_lookup
  module Project = Lsp_project
  module Project_cache = Lsp_project_cache
  module Document = Lsp_document
  module Server = Lsp_server
  module Loop = Lsp_server_loop
  module Picture_interp = Lsp_picture_interp
  module Request = Lsp_request.INTERNAL
  module Utils = Lsp_utils
  module Debug = Lsp_debug
end
