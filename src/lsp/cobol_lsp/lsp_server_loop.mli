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

(** [run ~platform ~cache_store_for ~config ~extensions] runs the server.

    [cache_store_for] is how a cache directory is opened.  An application with
    no file-system at hand passes {!Superbol_cache.Store.unavailable}, which
    disables caching: every lookup misses and nothing is stored. *)
val run
  : platform: Cobol_common.Platform.TYPES.platform
  -> config: Lsp_types.lsp_config
  -> extensions: extensions
  -> Lsp_server.exit_status
