(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Superbol_types

let show_error = function
  | Client_not_running ->
      Option.some @@
      Format.asprintf "The SuperBOL LSP client is not running; please retry \
                       after a COBOL file has been opened"
  | No_active_text_editor ->
      Option.some @@
      Format.asprintf "Found no active text editor"
  | _ ->
      None

let error_reporters =
  ref [show_error]

let register_error_reporter f =
  error_reporters := !error_reporters @ f

let show_error error =
  Option.value ~default:"Unknown internal error" @@
  List.find_map (fun show -> show error) !error_reporters

let show_error_message = function
  | Ok _ ->
      Promise.return ()
  | Error error ->
      let message = show_error error in
      let _ = Vscode.Window.showErrorMessage () ~message in
      Promise.return ()
