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

(** A source overlay associates complex source code elements with virtual limits
    of lexing tokens that can be fed to parsers so as to tag ASTs with complex
    source locations. *)

(** Source overlay limits, that {e MUST} be considered abstract and whose
    contents must not be inspected nor relied upon outside of this module.  Such
    limits are to be fed to menhir-generated parsers, that expect lexing
    positons from the {!Stdlib.Lexing} module, so unfortunately we can neither
    make this type abstract nor private.  *)
type limit = (* private *) Lexing.position

(** Manager of source overlay limits.  Includes some mutable state. *)
module type MANAGER = sig

  (** Identifier of the manager; may be used for debugging. *)
  val id: string

  (** [limits loc] creates and returns the left- and right-limit to the given
      location.

      Note: if [loc] overlaps with locations that have already been fed to this
      function, then {!restart} must first be called. *)
  val limits: Cobol_common.Srcloc.srcloc -> limit * limit

  (** [link_limits left right] links the right limit of a token [t] to the left
      limit of the subsequent token [t'] (that is to be fed to the parser right
      after [t]). *)
  val link_limits: limit -> limit -> unit

  (** [join_limits (start_limit, end_limit)] returns a source location that
      spans from [start_limit] to [end_limit].  [start_limit] must have been
      built (via {!limits}) {i before} [end_limit]. *)
  val join_limits: limit * limit -> Cobol_common.Srcloc.srcloc

  (** [dummy_limit] is a limit that may be fed to a parser or {!join_limits},
      but not given to {!link_limits}. *)
  val dummy_limit: limit

  (** [restart ()] instructs the manager that (a subset of) the limits
      previously created are now outdated and should not be relied upon. *)
  val restart: unit -> unit

end

(** Nanager module instantiation *)
module New_manager: functor (Id: sig val name: string end) () -> MANAGER

val debug_oc : out_channel option ref
