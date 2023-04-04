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

let activate (_extension: Vscode.ExtensionContext.t) =
  let str = Interop.Js.String.t_to_js "Hello from OCaml" in
  Js_of_ocaml.Firebug.console##log str;
  Promise.return ()

let () =
  Js_of_ocaml.Js.(export "activate" (wrap_callback activate))
