(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** An easy interface on top of the [Jsonm] library.

    This version provides more convenient (but far less flexible)
    input and output functions that go to and from [string] values.
    This avoids the need to write signal code, which is useful for
    quick scripts that manipulate JSON.

    More advanced users should go straight to the [Jsonm] library and
    use it directly, rather than be saddled with the Ezjsonm interface
    below.
*)

(** {2 Basic types} *)

type value =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list ]
(** JSON fragments. *)

type t =
  [ `A of value list
  | `O of (string * value) list ]
(** Well-formed JSON documents. *)

val value: t -> value
(** Cast a JSON well-formed document into a JSON fragment. *)

val wrap: value -> [> t]
(** [wrap v] wraps the value [v] into a JSON array. To use when it is
    not possible to statically know that [v] is a value JSON value. *)

val unwrap: t -> value
(** [unwrap t] is the reverse of [wrap]. It expects [t] to be a
    singleton JSON object and it return the unique element. *)

(** {2 Reading JSON documents and values} *)
val from_channel: in_channel -> [> t]
(** Read a JSON document from an input channel. *)

val from_string: string -> [> t]
(** Read a JSON document from a string. *)

val value_from_channel: in_channel -> value
(** Read a JSON value from an input channel. *)

val value_from_string: string -> value
(** Read a JSON value from a string. *)

val value_from_src: Jsonm.src -> value
(** Low-level function to read directly from a [Jsonm] source. *)

(** {2 Reading JSON documents and values -- with proper errors} *)

type error_location = (int * int) * (int * int)
(** Error locations in a source document follow the Jsonm representation
    of pairs of pairs [((start_line, start_col), (end_line, end_col))]
    with 0-indexed lines and 1-indexed columns. *)

type read_value_error = [
  | `Error of error_location * Jsonm.error
  | `Unexpected of [ `Lexeme of error_location * Jsonm.lexeme * string | `End_of_input ]
]
type read_error = [ read_value_error | `Not_a_t of value ]

val read_error_description : [< read_error ] -> string
(** A human-readable description of an error -- without using the error location. *)

val read_error_location : [< read_error ] -> error_location option
(** If the error is attached to a specific location in the buffer,
    return this location. *)

val from_channel_result: in_channel -> ([> t], [> read_error]) result
(** See {!from_channel}. *)

val from_string_result: string -> ([> t], [> read_error]) result
(** See {!from_string}. *)

val value_from_channel_result: in_channel -> (value, [> read_value_error]) result
(** See {!value_from_channel}. *)

val value_from_string_result: string -> (value, [> read_value_error]) result
(** See {!value_from_string}. *)

val value_from_src_result: Jsonm.src -> (value, [> read_value_error]) result
(** See {!value_from_src}. *)

(** {2 Writing JSON documents and values} *)

val to_channel: ?minify:bool -> out_channel -> t -> unit
(** Write a JSON document to an output channel. *)

val to_buffer: ?minify:bool -> Buffer.t -> t -> unit
(** Write a JSON document to a buffer. *)

val to_string: ?minify:bool -> t -> string
(** Write a JSON document to a string. This goes via an intermediate
    buffer and so may be slow on large documents. *)

val value_to_channel: ?minify:bool -> out_channel -> value -> unit
(** Write a JSON value to an output channel. *)

val value_to_buffer: ?minify:bool -> Buffer.t -> value -> unit
(** Write a JSON value to a buffer. *)

val value_to_string: ?minify:bool -> value -> string
(** Write a JSON value to a string. This goes via an intermediate
    buffer and so may be slow on large documents. *)

val value_to_dst: ?minify:bool -> Jsonm.dst-> value -> unit
(** Low-level function to write directly to a [Jsonm] destination. *)

(** {2 Constructors} *)

val unit: unit -> value
(** Same as [`Null]. *)

val bool: bool -> value
(** Same as [`Bool b]. *)

val string: string -> value
(** Same as [`String s]. *)

val strings: string list -> [> t]
(** Same as [`A [`String s1; ..; `String sn]]. *)

val int: int -> value
(** Same as [`Float (float_of_int i)]. *)

val int32: int32 -> value
(** Same as [`Float (Int32.to_float i)] *)

val int64: int64 -> value
(** Same as [`Float (Int64.to_float i)] *)

val float: float -> value
(** Some as [`Float f]. *)

val list: ('a -> value) -> 'a list -> [> t]
(** Build a list of values. *)

val option: ('a -> value) -> 'a option -> value
(** Either [`Null] or a JSON value. *)

val dict: (string * value) list -> [> t]
(** Build a dictionnary. *)

val pair: ('a -> value) -> ('b -> value) -> ('a * 'b) -> [> t]
(** Build a pair. *)

val triple: ('a -> value) -> ('b -> value) -> ('c -> value) ->
  ('a * 'b * 'c) -> [> t]
(** Build a triple. *)

(** {2 Accessors} *)

exception Parse_error of value * string
(** All the following accessor functions expect the provided JSON
    document to be of a certain kind. In case this is not the case,
    [Parse_error] is raised. *)

val get_unit: value -> unit
(** Check that the JSON document is [`Null]. *)

val get_bool: value -> bool
(** Extract [b] from [`Bool b]. *)

val get_string: value -> string
(** Extract [s] from [`String s]. *)

val get_strings: value -> string list
(** Extract [s1;..;sn] from [`A [`String s1; ...; `String sn]]. *)

val get_int: value -> int
(** Extract an integer. *)

val get_int32: value -> int32
(** Extract a 32-bits integer. *)

val get_int64: value -> int64
(** Extract a 64-bits integer. *)

val get_float: value -> float
(** Extract a float. *)

val get_list: (value -> 'a) -> value -> 'a list
(** Extract elements from a JSON array. *)

val get_option: (value -> 'a) -> value -> 'a option
(** Extract an optional document. *)

val get_dict: value -> (string * value) list
(** Extract the elements from a dictionnary document. *)

val get_pair: (value -> 'a) -> (value -> 'b) -> value -> ('a * 'b)
(** Extract the pair. *)

val get_triple: (value -> 'a) -> (value -> 'b) -> (value -> 'c) ->
    value -> ('a * 'b * 'c)
(** Extract the triple. *)

(** {2 High-level functions} *)

val mem: value -> string list -> bool
(** [mem v path] is true if the given path is valid for the JSON document [v]. *)

val find: value -> string list -> value
(** Find the sub-document addressed by the given path. Raise
    [Not_found] if the path is invalid. *)

val find_opt : value -> string list -> value option
(** Find the sub-document addressed by the given path. Returns
    [None] if the path is invalid. *)

val update: value -> string list -> value option -> value
(** Update the sub-document addressed by the given path. If the
    provided value is [None], then removes the sub-document. *)

val map: (value -> value option) -> value -> string list -> value
(** Apply a given function to a subdocument. *)

val encode_string: string -> value
(** Convert a (possibly non-valid UTF8) string to a JSON object.*)

val decode_string: value -> string option
(** Convert a JSON object to a (possibly non-valid UTF8)
    string. Return [None] if the JSON object is not a valid string. *)

val decode_string_exn: value -> string
(** Convert a JSON object to a (possibly non-valid UTF8) string. *)

val to_sexp: value -> Sexplib0.Sexp.t
(** Convert a JSON fragment to an S-expression. *)

val sexp_of_value: value -> Sexplib0.Sexp.t
(** An alias of [to_sexp] *)

val sexp_of_t: t -> Sexplib0.Sexp.t
(** Convert a JSON object to an S-expression *)

val of_sexp: Sexplib0.Sexp.t -> value
(** Convert an S-expression to a JSON fragment *)

val value_of_sexp: Sexplib0.Sexp.t -> value
(** AN alias of [of_sexp] *)

val t_of_sexp: Sexplib0.Sexp.t -> t
(** Convert an S-expression to a JSON object *)

(** {2 Error handling} *)

val parse_error: value -> ('a, unit, string, 'b) format4 -> 'a
(** Raise [Parse_error] *)
