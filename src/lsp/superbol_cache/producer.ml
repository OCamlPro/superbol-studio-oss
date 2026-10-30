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


(** The two identities the cache is versioned against.

    They answer different questions and move at different rates, so they are
    kept apart:

    - {!format_version} names the {e on-disk layout}.  It appears in the cache
      directory path, so that instances built against different layouts share a
      cache root without interfering.  Bump it by hand, and only when the layout
      changes.

    - {!tag} names the {e binary that produced a value}.  It takes part in every
      shallow key, so entries written by one producer are never even looked up
      by another. *)

(** Bumped by hand whenever the on-disk layout changes.  Never derived from the
    source, so that a rebuild does not orphan a usable cache. *)
let format_version = "v1"

(** Values are serialised with {!Marshal}, whose representation is tied to far
    more than the source revision.  Two users sharing a cache directory on one
    machine may well differ in every respect listed here --- distinct opam
    switches, or the native LSP alongside the one that runs under Node --- and
    reading one another's items would be undefined behaviour rather than a miss.
    So all of it takes part in the identity. *)
let description =
  let runtime = match Sys.backend_type with
    | Sys.Native -> "native"
    | Sys.Bytecode -> "bytecode"
    | Sys.Other name -> name                          (* e.g. "js_of_ocaml" *)
  in
  String.concat "\x00"
    [ Option.value Version.commit_hash ~default:Version.version;
      Version.version;
      Sys.ocaml_version;
      string_of_int Sys.word_size;
      runtime ]

(** Length of {!tag}, in bytes.  Fixed, so that a reader can always tell a tag
    mismatch from a truncated item. *)
let tag_length = 32

(** Hexadecimal digest of {!description}. *)
let tag =
  Digest.to_hex (Digest.string description)
