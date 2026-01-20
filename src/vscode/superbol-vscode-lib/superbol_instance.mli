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

type t
type client = Vscode_languageclient.LanguageClient.t

val make: lsp_server_prefix: string -> context:Vscode.ExtensionContext.t -> t
val subscribe_disposable: t -> Vscode.Disposable.t -> unit

val client: t -> client option
val context: t -> Vscode.ExtensionContext.t

val stop_language_server: t -> unit Promise.t
val start_language_server: t -> unit Promise.t
val start_autorestarter: t -> unit

(* --- *)

(** {2 Generic access functions} *)

val with_context_and_client
  : f:(context:Vscode.ExtensionContext.t -> client:client -> 'result)
  -> t
  -> (('a, Superbol_types.error) result Promise.t as 'result)

val lsp_request
  : meth:string
  -> data:Jsonoo.t
  -> t
  -> (Jsonoo.t, Superbol_types.error) result Promise.t

val current_document_uri
  : ?text_editor:Vscode.TextEditor.t
  -> unit
  -> Vscode.Uri.t option

module JSON: sig
  val current_text_document_id
    : Vscode.TextEditor.t
    -> Jsonoo.t
end

(* --- *)

val write_project_config
  : ?text_editor: Vscode.TextEditor.t
  -> t
  -> unit Promise.t

val get_project_config
  : t
  -> ((string, Jsonoo.t) Hashtbl.t, Superbol_types.error) result Promise.t
