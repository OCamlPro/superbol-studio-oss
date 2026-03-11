(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module Types: sig
  type superbol_instance = Superbol_instance.t
  include module type of Superbol_types
end
open Types

val activate
  : lsp_server_prefix:string
  -> Vscode.ExtensionContext.t
  -> superbol_instance Promise.t
val deactivate
  : unit
  -> unit Promise.t

module Printer = Superbol_printer
module Instance = Superbol_instance
module Workspace = Superbol_workspace
module Extension = Superbol_extension
