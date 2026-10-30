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

(** LSP server's main I/O-driven loop *)

module TYPES = struct
  type extensions =
    {
      alternate_request_handers: Lsp_request.TYPES.alternate_handler list;
    }
end
include TYPES

let default_extensions =
  {
    alternate_request_handers = [];
  }

(** Start the lsp server, listening and responding on stdin and stdout.  This is
    blocking (driven by standard input), and shutdown is triggered by a client
    request.  Returns [Ok ()] if the server ran and shut down properly, or
    [Error error_message] otherwise. *)
let run ~platform ~config
    ~extensions:{ alternate_request_handers = alternate_handlers } =
  Cobol_common.Platform.copy ~dst:Lsp_platform.record ~src:platform;
  Lsp_io.initialize_channels ();
  let rec loop state =
    match Lsp_io.read_message () with
    | Jsonrpc.Packet.Notification n ->
        continue @@ Lsp_notif.handle n state
    | Jsonrpc.Packet.Request r ->
        continue @@ reply @@
        Lsp_request.handle ~alternate_handlers r state
    | Jsonrpc.Packet.Batch_call calls ->
        batch_calls calls state
    | Jsonrpc.Packet.Response r ->
        continue @@ Lsp_response.handle r state
    | Jsonrpc.Packet.Batch_response resps ->
        batch_responses resps state
    | exception End_of_file ->
        Lsp_request.shutdown state;
        Error "Premature end of input stream"                    (* exit loop *)
    | exception Lsp_io.Parse_error msg ->
        Lsp_io.pretty_notification ~type_:Error "%s" msg;
        Error msg                                                (* exit loop *)
  and batch_calls calls state =
    match calls with
    | [] ->
        continue state
    | `Notification n :: calls' ->
        batch_calls calls' @@ Lsp_notif.handle n state
    | `Request n :: calls' ->
        batch_calls calls' @@ reply @@ Lsp_request.handle n state
  and batch_responses resps state =
    match resps with
    | [] ->
        continue state
    | resp :: resps ->
        batch_responses resps @@ Lsp_response.handle resp state
  and reply (state, response) =
    Lsp_io.send_response response;
    state
  and continue = function
    | Lsp_server.Exit code -> code                                (* exit loop *)
    | state -> loop state
  in
  loop (NotInitialized config)
