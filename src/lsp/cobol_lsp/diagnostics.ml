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

open Lsp_imports
open Ez_file.V1
open EzFile.OP

module DIAG = Cobol_common.Diagnostics

module TYPES = struct
  type diagnostics = Lsp.Types.Diagnostic.t list URIMap.t
end
include TYPES
type t = diagnostics

let pseudo_normalized_uri ~rootdir filename =
  let filename =
    let prefix = EzFile.current_dir_name // "" in
    if EzFile.is_absolute filename
    then filename
    else rootdir // match EzString.chop_prefix ~prefix filename with
      | None -> filename
      | Some x -> x
  in
  Lsp.Uri.of_path filename

let translate_one ~rootdir ~uri (diag: DIAG.t) =
  let uri, project_srcloc = match uri with
    | `Force uri ->
        uri, Cobol_common.Srcloc.lexloc_in ~filename:(Lsp.Uri.to_path uri)
    | `Main uri ->
        uri, Cobol_common.Srcloc.as_lexloc      (* rely on default projection *)
  in
  let uri, range =
    match Option.map project_srcloc (DIAG.location diag) with
    | Some (Lexing.{ pos_fname = f; _ }, _ as lexloc) ->
        pseudo_normalized_uri ~rootdir f, Position.range_of_lexloc lexloc
    | None ->
        uri, Position.pointwise_range_at_start
  in
  let diag =
    Lsp.Types.Diagnostic.create ()
      ~range
      ~severity:(match DIAG.severity diag with
          | Hint -> Lsp.Types.DiagnosticSeverity.Hint
          | Note | Info -> Information
          | Warn -> Warning
          | Error -> Error)
      ~message:Pretty.(to_string "%a" DIAG.pp_msg diag)
  in
  URIMap.singleton uri [diag]

let translate ~rootdir ~uri diagnostics =
  let init = match uri with `Force uri | `Main uri -> URIMap.singleton uri [] in
  DIAG.Set.fold begin fun diagnostic ->
    translate_one ~rootdir ~uri diagnostic |>
    URIMap.union (fun _uri a b -> Some (List.rev_append a b))
  end diagnostics init

let publish diagnostics : unit =
  URIMap.iter (fun uri diags -> Lsp_io.send_diagnostics ~uri diags) diagnostics

(* --- *)

(* NB: The following functions are currently not used anywhere, but the LSP
   could actually send more notificatons in the future. *)

let as_notification ?(log = false) diag =
  let type_ = match DIAG.severity diag with
    | Hint | Note -> Lsp.Types.MessageType.Log
    | Info -> Info
    | Warn -> Warning
    | Error -> Error
  in
  let message = Pretty.(to_string "%t%a" blast_margin DIAG.pp_msg diag) in
  if log then
    let params = Lsp.Types.LogMessageParams.create ~message ~type_ in
    Lsp.Server_notification.LogMessage params
  else
    let params = Lsp.Types.ShowMessageParams.create ~message ~type_ in
    Lsp.Server_notification.ShowMessage params

let as_notifications ?log diags =
  DIAG.Set.fold (fun diag -> List.cons (as_notification ?log diag)) diags []
