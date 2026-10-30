(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)


(** Collecting the witnesses a piece of work observes.

    Everything the COBOL preprocessor learns about the outside world, it learns
    through a {!Cobol_common.Platform} record: the files it reads, the
    directories it searches for copybooks, the environment variables that
    [>>IF SET] consults.  So rather than threading an accumulator through the
    preprocessor, a recorder simply {e wraps} that record, notes what passes
    through, and hands the witnesses back afterwards.

    Typical use:

    {[
      let recorder = Recorder.create ~platform in
      let result = preprocess ~platform:(Recorder.platform recorder) input in
      Cache.store cache ~key ~label ~witnesses:(Recorder.witnesses recorder)
        ~write_item result
    ]} *)

type t

(** [create ~platform] is a recorder over [platform]. *)
val create: platform:Cobol_common.Platform.TYPES.platform -> t

(** [platform t] is [t]'s platform: it behaves exactly like the one [t] was
    created over, and additionally records what it is asked. *)
val platform: t -> Cobol_common.Platform.TYPES.platform

(** [witnesses t] is everything observed so far, in no particular order and
    without duplicates. *)
val witnesses: t -> Types.witness list

(** [add t witness] records [witness] explicitly, for what the platform does not
    see --- the project configuration, say, or {!Cache.sub_witness} for an entry
    being reused. *)
val add: t -> Types.witness -> unit

(** [resolver ~cache ~platform ?contents_of ()] is the counterpart of a
    recorder: the [resolve] that {!Cache.lookup} expects, answering from
    [platform].

    [contents_of] is consulted first for the contents of a file, and is how an
    editor answers from a document it holds open rather than from what is on
    disk.  It is handed the {e canonical} spelling of the path, as
    {!Cache.canonical_path} produces it.  That is what makes an entry recorded against an unsaved buffer and
    one recorded against a file name interchangeable whenever their contents
    agree. *)
val resolver
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?contents_of: (string -> string option)
  -> unit
  -> (Types.witness -> bool)
