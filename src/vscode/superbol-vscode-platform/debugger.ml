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

let debugConfigurationProvider =
  let resolveDebugConfiguration ~folder:_ ~debugConfiguration ?token:_ () =
    let program = Vscode.DebugConfiguration.get_property debugConfiguration "program" in
    if Ojs.is_null program then
      Vscode.DebugConfiguration.set_property debugConfiguration "program"  ([%js.of: string] "${file}");
    `Value (Some debugConfiguration)
  in
  let resolveDebugConfigurationWithSubstitutedVariables ~folder:_ ~debugConfiguration:_ ?token:_ () =
    `Value None
  in
  let provideDebugConfigurations ~folder:_ ?token:_ () =
    let configuration =
      Vscode.DebugConfiguration.create ()
        ~name:"Launch"
        ~type_:"cobol"
        ~request:"launch"
        ~properties:[
          "program", ([%js.of: string] "${fileBasenameNoExtension}")
        ]
    in
    `Value (Some [configuration])
  in
  Vscode.DebugConfigurationProvider.create
    ~resolveDebugConfiguration
    ~provideDebugConfigurations
    ~resolveDebugConfigurationWithSubstitutedVariables

(* let inline_debug_adapter_factory =
  let createDebugAdapterDescriptor ~session:_ ~executable:_ =
    let implementation = () in
    `Value (
      Some (`DebugAdapterInlineImplementation
              (Vscode.DebugAdapterInlineImplementation.make ~implementation)))
  in
  Vscode.DebugAdapterDescriptorFactory.create ~createDebugAdapterDescriptor *)
