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

(** Simple communication functions for the LSP server to send an receive json
    RPC messages. *)

(** [send_out msg] returns {!type:unit} and is used to send out a message. This
    function can be edited later to use other channels than stdin or stdout as
    IO. *)
let send_out msg = print_string msg

(** [read_message ()] tries to read a json RPC message from the standard input
    stream.  Returns [Ok packet] upon success, or [Error error_response] if the
    message is in the wrong format. *)
let read_message () : (Jsonrpc.Packet.t, Jsonrpc.Response.Error.t) result =
  let rec read_headers acc =
    let line = read_line () in
    match String.trim line with
    | "" -> Ok acc (* an empty line after the headers *)
    | line ->
        try
          let i = String.index line ':' in
          if String.get line (i + 1) <> ' ' then raise Not_found;
          let header_key = String.lowercase_ascii @@ String.sub line 0 i in
          let header_value =
            String.lowercase_ascii @@ String.trim @@
            String.sub line (i + 1) (String.length line - i - 1)
          in
          read_headers @@ (header_key, header_value)::acc
        with _ -> Error (Fmt.str "invalid header: %s" line)
  in
  let rec read_len buf i len =
    if len > 0 then
      let n = input stdin buf i len in
      read_len buf (i + n) (len - n)
  in
  let headers = read_headers [] in
  match headers with
  | Ok headers ->
      begin match List.assoc_opt "content-type" headers, List.assoc_opt "content-length" headers with
        | (Some ("utf8" | "utf-8") | None), Some len_str ->
            let content_length = int_of_string_opt len_str in
            begin match content_length with
              | Some len ->
                  let buf = Bytes.make len '\000' in
                  let () = read_len buf 0 len in
                  let str = Bytes.to_string buf in
                  begin try
                      let json = Yojson.Safe.from_string str in
                      Ok (Jsonrpc.Packet.t_of_yojson json)
                    with Yojson.Json_error msg ->
                      let message = Fmt.str "invalid json: %s:\n%s" msg str in
                      Error (Jsonrpc.Response.Error.(make ~code:Code.ParseError ~message ()))
                  end
              | None ->
                  let message = Fmt.str "not an integer value: %s" len_str in
                  Error (Jsonrpc.Response.Error.(make ~code:Code.ParseError ~message ()))
            end
        | Some _, _ ->
            Error
              (Jsonrpc.Response.Error.(
                  make ~code:Code.InvalidRequest ~message:"content-type must be 'utf-8'" ()))
        | _, None ->
            Error
              (Jsonrpc.Response.Error.(
                  make ~code:Code.ParseError ~message:"missing content-length header" ()))
      end
  | Error message ->
      Error
        (Jsonrpc.Response.Error.(make ~code:Code.ParseError ~message ()))

(** [send_json json] sends out a json rpc package. *)
let send_json json =
  let str = Yojson.Safe.to_string json in
  let len = String.length str in
  let msg = Fmt.str "Content-Length: %d\r\n\r\n%s" len str in
  send_out msg

(** [send_response response] sends out a json RPC response on standard
    output. *)
let send_response response =
  let json = Jsonrpc.Response.yojson_of_t response in
  send_json json

(** [send_notification notif] sends out a json RPC notification on standard
    output. *)
let send_notification notif =
  let json = Jsonrpc.Notification.yojson_of_t notif in
  send_json json

(** [send_diagnostics ~uri diagnostics] sends out a list of diagnostics that
    pertain to the given URI. *)
let send_diagnostics ~uri diagnostics =
  let params = Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri () in
  let notif = Lsp.Server_notification.PublishDiagnostics params in
  send_notification (Lsp.Server_notification.to_jsonrpc notif)

(** [pretty_notification ~log ~type_ fmt ...] formats any number of arguments
    according to the format string [fmt], and sends the result via a json RPC
    notification for log or direct display (depending on the flag [log], false
    by default).  Also prints the result on stderr (for now). *)
let pretty_notification ?(log = false) ~type_ fmt =
  Pretty.string_to begin fun message ->
    Pretty.error "Notif: %s@." message;
    let notif =
      if log
      then Lsp.(Server_notification.LogMessage
                  (Types.LogMessageParams.create ~message ~type_))
      else Lsp.(Server_notification.ShowMessage
                  (Types.ShowMessageParams.create ~message ~type_))
    in
    send_notification (Lsp.Server_notification.to_jsonrpc notif)
  end ("%t@[<h>"^^fmt^^"@]") Pretty.blast_margin
