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

module TYPES: sig
  type severity = Diagnostics_sigs.severity =
    | Hint
    | Note
    | Info
    | Warn
    | Error

  type diagnostic
  type diagnostics

  type 'a in_result = ('a * diagnostic option, diagnostic option) result

  type 'a with_diags =
    {
      result: 'a;
      diags: diagnostics;
    }
end
include module type of TYPES
  with type severity = TYPES.severity
   and type diagnostic = TYPES.diagnostic
   and type diagnostics = TYPES.diagnostics
   and type 'a in_result = 'a TYPES.in_result
   and type 'a with_diags = 'a TYPES.with_diags
type t = diagnostic

val pp: t Pretty.printer
val pp_msg: t Pretty.printer

val message: t -> Pretty.delayed
val severity: t -> severity
val location: t -> Srcloc.srcloc option

(** {1 Set of diagnostics} *)

module Set: sig
  (** Persistent set of diagnostics *)
  type t = diagnostics
  val pp: t Pretty.printer
  val pp_above: level:severity -> t Pretty.printer
  val none: t
  val one: diagnostic -> t
  val maybe: diagnostic option -> t
  val two: diagnostic -> diagnostic -> t
  val cons: diagnostic -> t -> t
  val union: t -> t -> t
  val has_errors: t -> bool
  val map: (diagnostic -> 'a) -> t -> 'a list
  val fold: (diagnostic -> 'a -> 'a) -> t -> 'a -> 'a

  include Diagnostics_sigs.REPORT
    with type 'a t := ?loc:Srcloc.srcloc -> ('a, t) Pretty.func
     and type blind := diagnostic -> t

  (** Sets of diagnostics with values that do not contain any closure or module,
      and that can thus be marshalled/unmarshalled safely.

      The ability to perform delayed formatting is lost when translating from
      "regular" diagnostics into serializable ones, as the translation applies
      the formatting as if into a string without any right-margin.  *)
  type serializable
  val apply_delayed_formatting: t -> serializable
  val of_serializable: serializable -> t
end

(** {1 Functional and imperative interfaces to diagnostics} *)

module One: Diagnostics_sigs.REPORT
  with type 'a t := ?loc:Srcloc.srcloc -> ('a, t) Pretty.func
   and type blind := t -> t

module Now: Diagnostics_sigs.REPORT
  with type 'a t := Format.formatter -> ?loc:Srcloc.srcloc -> 'a Pretty.proc
   and type blind := Format.formatter -> t -> unit

module Acc: Diagnostics_sigs.REPORT
  with type 'a t := (Set.t as 's) -> ?loc:Srcloc.srcloc -> ('a, 's) Pretty.func
   and type blind := Set.t -> t -> Set.t

module Cont: Diagnostics_sigs.KREPORT
  with type ('a, 'b) t := (t -> 'b) -> ?loc:Srcloc.srcloc -> ('a, 'b) Pretty.func
   and type 'b kblind := (t -> 'b) -> t -> 'b

(** Allow direct access to persistent diagnostics reporting *)
include module type of Acc
include module type of Cont

(* --- *)

val result: ?diags:diagnostics -> 'a -> 'a with_diags
(* val result_only: 'a with_diags -> 'a *)
(* val with_diag: 'a -> diagnostic -> 'a with_diags *)
(* val with_diags: 'a -> diagnostics -> 'a with_diags *)
val with_more_diags: diags:diagnostics -> 'a with_diags -> 'a with_diags
(* val simple_result: 'a -> 'a with_diags *)
val some_result: ?diags:diagnostics -> 'a -> 'a option with_diags
val no_result: diags:diagnostics -> _ option with_diags
val map_result: f:('a -> 'b) -> 'a with_diags -> 'b with_diags
val map2_results: f:('a -> 'b -> ('c with_diags as 'x)) -> 'a with_diags -> 'b with_diags -> 'x
val map_some_result: f:('a -> 'b) -> 'a option with_diags -> 'b option with_diags
val more_result: f:('a -> 'b with_diags) -> 'a with_diags -> 'b with_diags
val cons_option_result: 'a option with_diags -> 'a list with_diags -> 'a list with_diags
val forget_result: _ with_diags -> diagnostics
(* val merge_results: f:('a -> 'b -> 'c) -> 'a with_diags -> 'b with_diags -> 'c with_diags *)
val show_n_forget: ?set_status:bool -> ?min_level:severity ->
  ?ppf:Format.formatter -> 'a with_diags -> 'a
val sink_result: ?set_status:bool -> ?ppf:Format.formatter -> _ with_diags -> unit

val hint_result: 'a -> ?loc:Srcloc.srcloc -> ('b, 'a with_diags) Pretty.func
val note_result: 'a -> ?loc:Srcloc.srcloc -> ('b, 'a with_diags) Pretty.func
val info_result: 'a -> ?loc:Srcloc.srcloc -> ('b, 'a with_diags) Pretty.func
val warn_result: 'a -> ?loc:Srcloc.srcloc -> ('b, 'a with_diags) Pretty.func
val error_result: 'a -> ?loc:Srcloc.srcloc -> ('b, 'a with_diags) Pretty.func

(* --- *)

module type STATEFUL = Diagnostics_sigs.STATEFUL
  with type blind := t -> unit
   and type diagnostics := diagnostics
   and type 'a with_diags := 'a with_diags

module type STATEFUL0 = Diagnostics_sigs.STATEFUL0
  with type blind := t -> unit
   and type diagnostics := diagnostics
   and type 'a with_diags := 'a with_diags

module MakeStateful:
  functor (H: sig val history: diagnostics end) -> STATEFUL0

module InitStateful:
  functor () -> STATEFUL0

(* --- *)

module Fatal: Diagnostics_sigs.FATAL
  with type ('a, 'b) with_optional_location :=
    ?loc:(Srcloc.srcloc)
    -> ('a, Format.formatter, unit, 'b) format4 -> 'a
