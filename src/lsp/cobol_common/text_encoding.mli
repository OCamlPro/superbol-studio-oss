(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Decoding source files into UTF-8, as VS Code does.

    The LSP server receives documents from VS Code as UTF-8 text, decoded
    according to the [files.encoding] setting.  Analyses are keyed on the
    contents of documents, so a file read from disk must be decoded the same
    way, byte for byte, for its analysis to be shared with the server.

    Encodings are designated by the identifiers VS Code uses for
    [files.encoding] (e.g. ["utf8"], ["windows1252"], ["cp850"]).  Multi-byte
    encodings other than UTF-8 and UTF-16 are not supported. *)

type t

(** [of_name name] is the encoding VS Code designates with [name]; or else an
    error message. *)
val of_name: string -> (t, string) result

(** [name enc] is the VS Code identifier of [enc]. *)
val name: t -> string

(** VS Code identifiers of all supported encodings. *)
val supported_names: string list

(** [decode enc bytes] is the UTF-8 text VS Code gets when it opens a file that
    holds [bytes], with [files.encoding] set to [enc]:

    - a byte order mark (UTF-8, UTF-16LE, or UTF-16BE) takes precedence over
      [enc], and is removed;
    - ill-formed sequences, and bytes that [enc] leaves undefined, are
      replaced with U+FFFD (in UTF-8 and UTF-16, one per maximal subpart of a
      sequence); an odd byte at the end of UTF-16 input is dropped.

    VS Code's guesses ([files.autoGuessEncoding], or detecting UTF-16 without
    byte order mark) are not reproduced.

    [bytes] is returned as is whenever it is already the result, so decoding
    ASCII or well-formed UTF-8 input does not allocate. *)
val decode: t -> string -> string
