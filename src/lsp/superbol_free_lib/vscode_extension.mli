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

open Vscode_json.Manifest

val package: package
val languages: language list_or_one
val grammars: grammar list_or_one
val debuggers: debugger list_or_one
val breakpoints: breakpoint list_or_one
val configuration: configuration
val taskDefinitions: taskDefinition list_or_one
val configurationDefaults: (string * configurationDefault) list_or_one
val problemPatterns: problemPattern list_or_one
val problemMatchers: problemMatcher list_or_one
val commands: command list_or_one
val menus: (string * menu list_or_one) list_or_one
val contributes: contributes
val manifest: vscode
