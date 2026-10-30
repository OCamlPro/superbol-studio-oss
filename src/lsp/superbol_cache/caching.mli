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


(** Encoding of individual cached items.

    An item is a producer tag followed by a payload.  Reading one back checks
    the tag first, so that items left behind by another producer are skipped
    instead of being misread.

    {2 Trust}

    The payload codecs below are built on {!Marshal}, which is {b not} safe on
    input it did not itself produce: a forged payload can crash the reader or
    make it execute arbitrary code.  The tag guards against accident --- a
    rebuild, a different switch, a different runtime --- and never against
    malice.  A cache directory shared between users is therefore only sound
    between users who already trust one another. *)

(** [encode ?tag payload] frames [payload] for storage. *)
val encode: ?tag:string -> string -> string

(** [decode ?tag item] checks the tag framing [item] and returns its payload. *)
val decode: ?tag:string -> string -> (string, Types.read_error) result

(** [marshal v] is the payload encoding of [v], via {!Marshal}.

    Values holding closures are rejected rather than serialised with
    [Marshal.Closures]: such a payload is only valid for the byte-identical
    executable, which neither the producer tag nor anything else the cache can
    observe is able to establish.  Callers are expected to cache a description
    from which such a value can be rebuilt, rather than the value itself. *)
val marshal: 'a -> (string, Types.read_error) result

(** [unmarshal payload] reads back a value written by {!marshal}.

    Unsafe on a payload written by a different producer; see the note on trust
    above.  The caller is responsible for the type at which the result is
    used --- {!Marshal} cannot check it. *)
val unmarshal: string -> ('a, Types.read_error) result

(** [write_item v] is [marshal v] framed by {!encode}. *)
val write_item: 'a -> (string, Types.read_error) result

(** [read_item item] is the inverse of {!write_item}. *)
val read_item: string -> ('a, Types.read_error) result
