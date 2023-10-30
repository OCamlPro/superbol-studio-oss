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

open Srcloc

module TYPES = struct
  type severity = Diagnostics_sigs.severity =
    | Hint                                   (** hint *)
    | Note                                   (** general note *)
    | Info                                   (** informative message *)
    | Warn                                   (** warning *)
    | Error                                  (** unrecoverable error *)

  type diagnostic =
    {
      severity: severity;
      message: Pretty.delayed;
      location: srcloc option;
      stamp: int;
    }

  type diagnostics = diagnostic list                    (* reversed, abstract *)

  type 'a in_result = ('a * diagnostic option, diagnostic option) result

  type 'a with_diags =
    {
      result: 'a;
      diags: diagnostics;
    }
end
include TYPES
type t = diagnostic

let mk =
  let current_stamp = ref 0 in
  fun severity location message ->
    let stamp = !current_stamp in
    incr current_stamp;
    { severity; message; location; stamp }

let pp ppf { severity; message; location = loc; _ } =
  let prefix, style = match severity with
    | Hint -> "Hint", `Fg `Cyan
    | Note -> "Note", `None
    | Info -> "Info", `Fg `Green
    | Warn -> "Warning", `Fg `Yellow
    | Error -> "Error", `Fg `Red
  in
  Msgs.pp_msg ?loc ppf "%a" begin
    Pretty.styles [`Bold; style] @@ fun ppf ->
    Pretty.print ppf ">> %s: @[%t@]@\n" prefix
  end message

let is_error = function
  | { severity = Error; _ } -> true
  | _ -> false

let compare_severity a b =
  if a = b then 0 else match a, b with
    | Hint, _ -> -1 | _, Hint -> 1
    | Note, _ -> -1 | _, Note -> 1
    | Info, _ -> -1 | _, Info -> 1
    | Warn, _ -> -1 | _, Warn -> 1
    | Error, _ -> -1

let pp_msg ppf diag = diag.message ppf
let message diag = diag.message
let severity diag = diag.severity
let location diag = diag.location

(* --- *)

module One = struct
  (** Reporting module where each function builds and returns a single
      diagnostic. *)
  let diag severity ?loc = Pretty.delayed_to (mk severity loc)
  let hint ?loc fmt = diag Hint ?loc fmt
  and note ?loc fmt = diag Note ?loc fmt
  and info ?loc fmt = diag Info ?loc fmt
  and warn ?loc fmt = diag Warn ?loc fmt
  and error ?loc fmt = diag Error ?loc fmt
  and blind = Fun.id
end

module Now = struct
  (** Reporting module where each value is a procedure that prints a single
      diagnostic on a given formatter. *)
  let diag severity ppf ?loc fmt =
    Pretty.delayed_to (fun message -> pp ppf (mk severity loc message)) (fmt^^"@.")
  let hint ppf = diag Hint ppf
  and note ppf = diag Note ppf
  and info ppf = diag Info ppf
  and warn ppf = diag Warn ppf
  and error ppf = diag Error ppf
  and blind = pp
end

module Cont = struct
  (** Reporting module where each function gives diagnostics to a continuation
      function. *)
  let kdiag severity k ?loc =
    Pretty.delayed_to (fun message -> k (mk severity loc message))
  let khint s = kdiag Hint s
  and knote s = kdiag Note s
  and kinfo s = kdiag Info s
  and kwarn s = kdiag Warn s
  and kerror s = kdiag Error s
  and kblind = ( @@ )
end

(* --- *)

module Set = struct
  (** Persistent sets of diagnostics *)

  type t = diagnostics                                            (* reversed *)

  let sort diags =
    List.sort (fun { stamp = s1; _ } { stamp = s2; _ } -> Int.compare s1 s2) diags

  (* TODO: order via locs and/or a global timestamp? something more
     intricate? *)
  let pp ppf diags =
    Pretty.list ~fopen:"@[<v>" ~fclose:"@]@\n" ~fsep:"@\n" ~fempty:""
      pp ppf (sort diags)
  let pp_above ~level ppf diags =
    pp ppf @@
    List.filter (fun { severity; _ } -> compare_severity level severity <= 0) diags
  let none: t = []
  let one d = [d]
  let two d d' = [d; d']
  let maybe = Option.fold ~some:one ~none
  let cons = List.cons
  let union = List.rev_append
  let has_errors = List.exists is_error
  let map f diags = List.map f (sort diags)
  let fold f diags acc = List.fold_left (fun a d -> f d a) acc (sort diags)

  let diag s = Cont.kdiag s one
  let hint ?loc = Cont.khint one ?loc
  let note ?loc = Cont.knote one ?loc
  let info ?loc = Cont.kinfo one ?loc
  let warn ?loc = Cont.kwarn one ?loc
  let error ?loc = Cont.kerror one ?loc
  let blind = Cont.kblind one

  (** Sets of diagnostics with values that do not contain any closure or module,
      and that can thus be marshalled/unmarshalled safely.

      The ability to perform delayed formatting is lost when translating from
      "regular" diagnostics into serializable ones, as the translation applies
      the formatting as if into a string without any right-margin.  *)
  type serializable = serializable_diagnostic list
  and serializable_diagnostic = {
    sd_severity: severity;
    sd_msg: string;
    sd_loc: srcloc option;
  }

  let apply_delayed_formatting: t -> serializable =
    map begin fun { severity; message; location; _ } ->
      { sd_severity = severity;
        sd_msg = Pretty.(to_string "%t@[<h>%t@]" blast_margin) message;
        sd_loc = location }
    end
  let of_serializable: serializable -> t =
    List.map begin fun { sd_severity; sd_msg; sd_loc } ->
      mk sd_severity sd_loc (Pretty.delayed "%s" sd_msg)
    end
end
type diagnostics = Set.t

(* --- *)

module Acc = struct
  (** Reporting module where each functions adds diagnostics into a given
      set. *)
  let diag severity (diags: diagnostics) ?loc =
    Pretty.delayed_to (fun message -> Set.cons (mk severity loc message) diags)
  let hint s = diag Hint s
  and note s = diag Note s
  and info s = diag Info s
  and warn s = diag Warn s
  and error s = diag Error s
  and blind s d = Set.cons d s
end

include Acc
include Cont

(* --- *)

let result ?(diags = Set.none) result = { result; diags }
let result_only { result; _ } = result
let with_diag r d = result ~diags:(Set.one d) r
let with_diags r diags = result ~diags r
let with_more_diags ~diags { result; diags = diags' } =
  { result; diags = Set.union diags' diags }
let simple_result r = result r
let some_result ?diags r = result ?diags (Some r)
let no_result ~diags = { result = None; diags }
let map_result ~f { result; diags } = { result = f result; diags }
let more_result ~f { result; diags } = with_more_diags ~diags (f result)
let map2_results ~f r1 r2 =
  more_result ~f:(f r1.result) (with_more_diags ~diags:r1.diags r2)
let map_some_result ~f =
  map_result ~f:(Option.map f)
let cons_option_result = function
  | { result = None; diags } -> with_more_diags ~diags
  | { result = Some r; diags } -> more_result ~f:(fun tl -> result ~diags @@ r :: tl)
let forget_result { diags; _ } = diags
let merge_results ~f r1 r2 =
  result (f r1.result r2.result) ~diags:(Set.union r1.diags r2.diags)
let show_n_forget ?(set_status = true) ?(min_level = Hint)
    ?(ppf = Fmt.stderr) { result; diags } =
  Set.pp_above ~level:min_level ppf diags;
  if set_status && Set.has_errors diags then Exit_status.raise ();
  result
let sink_result ?set_status ?ppf r =
  ignore @@ show_n_forget ?set_status ?ppf r


let hint_result r = Cont.khint (with_diag r)
let note_result r = Cont.knote (with_diag r)
let info_result r = Cont.kinfo (with_diag r)
let warn_result r = Cont.kwarn (with_diag r)
let error_result r = Cont.kerror (with_diag r)

(* --- *)

(** Type of modules that encapsulate an internal, mutable, set of diagnostics.
    See {!Cobol_common.catch_diagnostics} and
    {!Cobol_common.catch_n_show_diagnostics} for typical usage. *)
module type STATEFUL = Diagnostics_sigs.STATEFUL
  with type blind := t -> unit
   and type diagnostics := diagnostics
   and type 'a with_diags := 'a with_diags

module type STATEFUL0 = Diagnostics_sigs.STATEFUL0
  with type blind := t -> unit
   and type diagnostics := diagnostics
   and type 'a with_diags := 'a with_diags

(** Initializes a stateful diagnostics reporting module based on the given
    history. *)
module MakeStateful (H: sig val history: diagnostics end) = struct
  let diags = ref H.history
  let blind d = diags := d :: !diags
  let add_all set = diags := List.rev_append set !diags
  let inspect ~reset =
    let res = !diags in
    if reset then diags := [];
    res
  let diag severity ?loc =
    Pretty.delayed_to (fun message -> blind (mk severity loc message))
  let hint ?loc = diag Hint ?loc
  and note ?loc = diag Note ?loc
  and info ?loc = diag Info ?loc
  and warn ?loc = diag Warn ?loc
  and error ?loc = diag Error ?loc
  let grab_diags { result; diags } = add_all diags; result
end

(** Initializes a stateful diagnostics reporting module based on an empty
    history. *)
module InitStateful () = MakeStateful (struct let history = Set.none end)

(* --- *)

module Fatal = struct
  (** Fatal errors from which we cannot recover. *)
  let localized_error = Msgs.localized_error
  let error           = Msgs.error
  (* exception Sink of Set.t *)

  (* (\** Fail with a set of diagnostics *\) *)
  (* let sink s = raise @@ Sink s *)
end

let of_exn: exn -> diagnostic = function
  | Msgs.LocalizedError (s, loc, _) ->                        (* TODO: addenda *)
      One.error ~loc "%t" s
  | Msgs.Error msg ->
      One.error "%t" msg
  | Failure msg
  | Sys_error msg ->
      One.error "%s" msg
  | e ->
      raise e
