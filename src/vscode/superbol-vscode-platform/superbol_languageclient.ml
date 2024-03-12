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

module LSP = Vscode_languageclient


(* Helpers to find the bundled superbol executable *)
let rec find_existing = function
  | [] -> raise Not_found
  | uri :: uris ->
    if Node.Fs.existsSync (Vscode.Uri.fsPath uri) then uri
    else find_existing uris


(* Look for the most specific `superbol-free` executable amongst (in order):

  - `superbol-free-${platform}-${arch}${suffix}`
  - `superbol-free-${platform}${suffix}`
  - `superbol-free${suffix}`

  The `platform` and `arch` used are from the corresponding `process`
  attributes in node.js. The `suffix` is `".exe"` on Windows, and empty
  otherwise.

  https://nodejs.org/api/process.html#processplatform
  https://nodejs.org/api/process.html#processarch
*)
let find_superbol root =
  let open Node.Process in
  let prefix = "superbol-free" in
  let suffix = if platform == "win32" then ".exe" else "" in
  Vscode.Uri.fsPath @@ find_existing @@ List.map (fun name ->
    Vscode.Uri.joinPath root ~pathSegments:[name]) @@ [
    Format.asprintf "%s-%s-%s%s" prefix platform arch suffix;
    Format.asprintf "%s-%s%s" prefix platform suffix;
    Format.asprintf "%s%s" prefix suffix
  ]


let serverOptions ~context =
  let bundled_superbol =
    try
      find_superbol
        (Vscode.Uri.joinPath ~pathSegments:["_dist"]
           (Vscode.ExtensionContext.extensionUri context));
    with Not_found ->
      (* If there is no bundled executable for the current platform, fall back
         to looking for superbol-free in the PATH *)
      "superbol-free"
  in
  LSP.ServerOptions.create ()
    ~command:(match Superbol_workspace.superbol_exe () with
        | None -> bundled_superbol
        | Some cmd -> cmd)
    ~args:["lsp"]


let clientOptions () =
  LSP.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (LSP.DocumentFilter.createLanguage ()
                 ~language:"cobol");
    |]
