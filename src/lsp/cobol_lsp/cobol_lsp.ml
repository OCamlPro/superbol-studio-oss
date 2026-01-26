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

(** {2 Modules and functions exported for extensibility purposes} *)

module Types = struct
  include Lsp_imports
  include Lsp_server.TYPES
  include Lsp_diagnostics.TYPES
  include Lsp_lookup.TYPES
  include Lsp_document.TYPES
  include Lsp_project.TYPES
  include Lsp_project_cache.TYPES
  include Lsp_request.TYPES
end

module Server = Lsp_server
module Project = Lsp_project
module Document = Lsp_document
module Diagnostics = Lsp_diagnostics
module Request = Lsp_request
module Lookup = Lsp_lookup
module Utils = Lsp_utils
module Error = Lsp_error
module IO = Lsp_io

(* --- *)

(** {2 Modules and functions exported for testing purposes} *)

module INTERNAL = struct
  module Project_cache = Lsp_project_cache
  module Picture_interp = Lsp_picture_interp
  module Debug = Lsp_debug
end
