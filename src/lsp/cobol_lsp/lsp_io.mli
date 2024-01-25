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

exception Parse_error of string

(** [initialize_channels ()] sets stdin and stdout channels in binary mode; this
    is required for proper operations on some systems. *)
val initialize_channels: unit -> unit

(** [read_message ()] tries to read a json RPC message from the standard input
    stream.  Raises {!End_of_file} upon end of input stream. *)
val read_message: unit -> Jsonrpc.Packet.t

(** [send_response response] sends out a json RPC response on standard
    output. *)
val send_response: Jsonrpc.Response.t -> unit

(** [send_notification notif] sends out a json RPC notification on standard
    output. *)
val send_notification: Jsonrpc.Notification.t -> unit

(** [send_diagnostics ~uri diagnostics] sends out a list of diagnostics that
    pertain to the given URI. *)
val send_diagnostics: uri:Lsp.Uri.t -> Lsp.Types.Diagnostic.t list -> unit

(** [pretty_message ~log ~type_ fmt ...] formats any number of arguments
    according to the format string [fmt], and sends the result via a json RPC
    notification. *)
val pretty_message
  : ?log:bool
  -> type_:Lsp.Types.MessageType.t
  -> ('a, Format.formatter, unit) format -> 'a

(** [pretty_log ~type_ fmt ...] formats any number of arguments according to the
    format string [fmt], and sends the result as a log message via a json RPC
    notification. *)
val pretty_log: type_:Lsp.Types.MessageType.t -> _ Pretty.proc
val log: type_:Lsp.Types.MessageType.t -> _ Pretty.proc
val log_error: _ Pretty.proc
val log_warn: _ Pretty.proc
val log_info: _ Pretty.proc
val log_debug: _ Pretty.proc

(** [pretty_notification ~type_ fmt ...] formats any number of arguments
    according to the format string [fmt], and sends the result as a user
    notification message via a json RPC notification. *)
val pretty_notification: type_:Lsp.Types.MessageType.t -> _ Pretty.proc
val notification: type_:Lsp.Types.MessageType.t -> _ Pretty.proc
val notify_error: _ Pretty.proc
val notify_warn: _ Pretty.proc
val notify_info: _ Pretty.proc
