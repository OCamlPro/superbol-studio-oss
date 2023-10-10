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

module Basics = Basics
module Srcloc = Srcloc
module Copybook = Copybook
module Diagnostics = Diagnostics
module Visitor = Visitor
module Behaviors = Behaviors
module Tokenizing = Tokenizing

exception FatalError of string
val fatal: ('a, Format.formatter, unit, _) format4 -> 'a

module Types: sig

  (** Transitional (to be removed) *)
  type 'a res = ('a * Diagnostics.Set.t, Diagnostics.Set.t) result

  include module type of Diagnostics.TYPES
  include module type of Srcloc.TYPES

end
include module type of Types
  with type 'a res = 'a Types.res
   and type 'a with_diags = 'a Types.with_diags
   and type 'a with_loc = 'a Types.with_loc
   and type lexloc = Types.lexloc
   and type srcloc = Types.srcloc

(** [join_all diags res_l] takes every value of a list of {!type:('a, 'e) result}
    and return a {!type:('a, 'e) list result}. If all the elements in the parameter list
    are [Ok _] then the result is [Ok _] otherwise [Error _] *)
val join_all: ('a, 'e) result list -> ('a list, 'e) result

(** [join_any diags res_l] takes a list of result and makes it a list, the elements of the given
    list are added to the resulting list only if they are [Ok _]. *)
val join_any: ('a, 'e) result list -> 'a list

(** [catch_diagnostics ?ppf f x] enables [f] to fail abruptly with an exception
    while operating on an imperative (state-full) diagnostics module.

    It first creates a stateful diagnostics module [D] and calls [f D x] while
    intercepting common system exceptions (only {!Sys_error} for now) and fatal
    diagnostics.  An {!Error} is returned in the latter case, or whenever [f]
    terminates with an {!Error}.

    At last, the diagnostics accumulated in [D] are printed using [ppf]
    ([Format.err_formatter] by default) in case of a fatal error ({!fatal} is
    called and {!FatalError} is not caught --- which it shouldn't); in this case
    {!FatalError} is re-thrown afterwards.  *)
val catch_diagnostics
  : ?ppf:Format.formatter
  -> ((module Diagnostics.STATEFUL) -> 'c -> 'a res) -> 'c -> 'a res

(** [with_stateful_diagnostics ~f x] applies [f] on a fresh stateful diagnostics
    module [D] and [x], and returns the result combined with all diagnostics
    accumulated using [D].

    Any exception that may be raised and escape [f] is not caught, so it is the
    responsibility of [f] to catch it. *)
val with_stateful_diagnostics
  : f:((module Diagnostics.STATEFUL) -> 'b -> 'a) -> 'b -> 'a with_diags

val show_diagnostics
  : ?ppf:Format.formatter
  -> Diagnostics.Set.t -> unit


(** Exit the program with a status that depends on diagnostics reported in
    {!catch_n_show_diagnostics}.  *)
val exit: ?status:int -> unit -> _



val do_unit
  : ((module Diagnostics.STATEFUL) -> 'a -> unit)
  -> ?epf:Format.formatter
  -> 'a
  -> unit

val do_any
  : ((module Diagnostics.STATEFUL) -> 'a -> 'b)
  -> ?epf:Format.formatter
  -> 'a
  -> 'b
