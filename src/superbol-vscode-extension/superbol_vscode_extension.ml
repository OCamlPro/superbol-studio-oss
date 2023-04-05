(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)


(* If you delete or rename this file, you should add
   'src/superbol-vscode-extension/main.ml' to the 'skip' field in "drom.toml" *)

let client = ref None

let activate (_extension: Vscode.ExtensionContext.t) =
  let oc = Vscode.Window.createOutputChannel ~name:"superbol-vscode-extension" in
  Vscode.OutputChannel.appendLine oc ~value:"Hello from ocaml";
  let command = "/home/emilien/development/ocamlpro/padbol/superbol" in
  let args = ["x-lsp"] in
  let serverOptions = Vscode_languageclient.ServerOptions.create
                        ~command
                        ~args
                        ()
  in
  let documentSelector =
    [| `Filter (Vscode_languageclient.DocumentFilter.createLanguage ~language:"cobol" ()) |]
  in
  let clientOptions = Vscode_languageclient.ClientOptions.create ~documentSelector () in
  client :=
    Some (Vscode_languageclient.LanguageClient.make
            ~id:"cobolServer"
            ~name:"Cobol Server"
            ~serverOptions
            ~clientOptions
            ());
  match !client with
  | Some client -> Vscode_languageclient.LanguageClient.start client
  | None -> Promise.return ()

let deactivate () =
  match !client with
  | None -> Promise.return ()
  | Some client -> Vscode_languageclient.LanguageClient.stop client

let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate))
