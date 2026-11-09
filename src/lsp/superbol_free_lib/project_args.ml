(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

let get () =
  let project_root = ref None and encoding = ref None in
  let args = [
    ["project-root"; "root"], Arg.File (fun f -> project_root := Some f),
    EZCMD.info ~docv:"DIR" "Set the root of the project (autodetected on a \
                            per-file basis otherwise)";

    ["encoding"],
    Arg.String begin fun s ->
      (* Unsupported encodings are not an error, as the LSP server reads files
         as they are with those. *)
      encoding :=
        match Cobol_common.Text_encoding.of_name s with
        | Ok enc ->
            Some enc
        | Error msg ->
            Cobol_common.Diagnostics.Now.warn Fmt.stderr
              "%s;@ files@ are@ read@ as@ they@ are" msg;
            None
    end,
    EZCMD.info ~docv:"ENCODING"
      "Decode files as Visual Studio Code does with this value of its \
       `files.encoding' setting (e.g. `windows1252', `cp850').  Files are read \
       as they are by default, or (with a warning listing the supported \
       encodings) if ENCODING is not supported";
  ] in
  let given arg ~default = match arg with None -> default | given -> given in
  let get ?(setup = Caching_commands.default_setup) () =
    Caching_commands.{
      setup with
      project_root = given !project_root ~default:setup.project_root;
      encoding = given !encoding ~default:setup.encoding;
    }
  in
  get, args
