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

(** Modules signatures dedicated to diagnostics.

    Placed here to avoid duplications in ML & MLI *)

type severity =
  | Hint
  | Note
  | Info
  | Warn
  | Error

module type REPORT = sig
  type _ t and blind
  val diag: severity -> _ t
  val hint: _ t
  val note: _ t
  val info: _ t
  val warn: _ t
  val error: _ t
  val blind: blind
end

module type KREPORT = sig
  type (_, _) t and 'a kblind
  val kdiag: severity -> _ t
  val khint: _ t
  val knote: _ t
  val kinfo: _ t
  val kwarn: _ t
  val kerror: _ t
  val kblind: 'a kblind
end

module type STATEFUL = sig
  include REPORT
    with type 'a t := ?loc:Srcloc.srcloc -> 'a Pretty.proc
  type diagnostics
  type _ with_diags
  val add_all: diagnostics -> unit
  val grab_diags: 'a with_diags -> 'a
end

module type STATEFUL0 = sig
  include STATEFUL
  val inspect: reset:bool -> diagnostics
end

module type FATAL = sig
  type ('a, 'b) with_location
  type ('a, 'b) with_optional_location
  val localized_error: _ with_location [@@deprecated "Please try a proper \
                                                      recovery"]
  val error: _ with_optional_location
  (* type set *)
  (* exception Sink of set *)
  (* val sink: set -> _ *)
end
