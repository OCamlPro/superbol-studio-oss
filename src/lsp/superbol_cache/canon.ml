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

open Ez_file.V1

(** Canonicalisation of the paths that appear in witnesses.

    Witnesses only ever need to match on the machine that recorded them, so this
    performs no portable normalisation: it applies the conventions of the
    running system.  What it must guarantee is that two spellings of one path
    that the {e same} system considers equal --- notably differing only in case
    on Windows and macOS, or in separators, or in redundant [.] and [..]
    components --- canonicalise to one string.

    Symbolic links are deliberately left unresolved: doing so requires I/O
    outside the cache directory, which this library does not perform. *)

(** [strip_extraneous_components p] removes redundant [.] and [..] components
    from a path. *)
let strip_extraneous_components p =
  let absolute = EzFile.is_absolute p in
  (* A Windows drive or UNC prefix is kept verbatim as the first component, so
     that it is never eaten by a [..] resolution. *)
  let prefix, rest =
    if String.length p >= 2 && p.[1] = ':' &&
       (match p.[0] with 'a'..'z' | 'A'..'Z' -> true | _ -> false)
    then String.sub p 0 2, String.sub p 2 (String.length p - 2)
    else "", p
  in
  let body =
    String.concat "/" @@
    List.rev @@
    List.fold_left begin fun acc p ->
      match p, acc with
      | "", _ | ".", _ -> acc
      | "..", [] -> [".."]                      (* nothing to resolve against *)
      | "..", ".." :: _ -> ".." :: acc            (* keep piling up leading [..] *)
      | "..", _ :: tl -> tl
      | component, _ -> component :: acc
    end [] @@
    String.split_on_char '/' rest
  in
  if prefix <> "" then prefix ^ "/" ^ body
  else if absolute then "/" ^ body
  else if body = "" then "."
  else body

(** [path ~case_insensitive p] is the canonical form of [p].

    Separators are normalised to ['/'], repeated separators are collapsed, [.]
    components are dropped, and [..] components are resolved lexically against
    the preceding one.  Leading [..] components of a relative path are kept, as
    there is nothing to resolve them against.  A trailing separator is dropped,
    except on a root.  When [case_insensitive] holds, the result is lowercased.

    Note that a lexical [..] resolution is only sound in the absence of symbolic
    links; it matches the behaviour callers already get from the paths they
    build for copybook lookup. *)
let path ~case_insensitive p =
  let p =
    if String.contains p '\\'
    then String.map (function '\\' -> '/' | c -> c) p
    else p
  in
  let p = strip_extraneous_components p in
  if case_insensitive then String.lowercase_ascii p else p

(** [listing ~case_insensitive names] is the canonical rendering of a list of
    strings (like a directory listing), suitable for digesting.  Entries are
    canonicalised as single path components, deduplicated, and sorted, so that
    the digest does not depend on the order in which the underlying system
    happened to report them. *)
let listing ~case_insensitive names =
  List.rev_map (fun n -> if case_insensitive then String.lowercase_ascii n else n)
    names |>
  List.sort_uniq String.compare |>
  String.concat "\n"
