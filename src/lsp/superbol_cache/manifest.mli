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


(** Manifests: what was observed while producing an entry, and where its value
    went.

    Unlike values, manifests are encoded textually rather than with {!Marshal}.
    Every lookup reads one, and the pruner reads {e all} of them --- including,
    in a shared cache directory, those written by other users.  Decoding is
    therefore kept to a parser that cannot do anything worse than reject its
    input. *)

(** [shallow_key ~label ~inputs] is the key an entry is looked up by.

    It digests the producer tag together with [label] and [inputs], which are
    the digests of everything known {e before} the work is performed --- the
    primary input and the effective configuration.  Whatever is only discovered
    while working belongs in the manifest's witnesses instead. *)
val shallow_key: label:string -> inputs:Types.digest list -> Types.key

(** [encode manifest] renders [manifest] for storage. *)
val encode: Types.manifest -> string

(** [decode s] reads back a manifest rendered by {!encode}. *)
val decode: string -> (Types.manifest, Types.read_error) result
