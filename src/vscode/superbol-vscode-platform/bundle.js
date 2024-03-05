// -----------------------------------------------------------------------
//
//                        SuperBOL OSS Studio
//
//
//  Copyright (c) 2023 OCamlPro SAS
//
//  All rights reserved.
//  This source code is licensed under the MIT license found in the
//  LICENSE.md file in the root directory of this source tree.
//
// -----------------------------------------------------------------------
//
// This file is copied as `_out/superbol-vscode-platform-bundle.js`
// before bundling the VS Code extension.  Its sole purpose is to
// bundle the "main" LSP client extension together with the debugger
// extension that we import as a submodule in
// `import/superbol-vscode-debug`.
// 
import * as main from "./superbol_vscode_platform.bc";
import * as debug from "../import/superbol-vscode-debug/src/extension";
export function activate (context) {
  main.activate (context);
  debug.activate (context);
}
export function deactivate (context) {
  debug.deactivate (context);
  main.deactivate (context);
}

