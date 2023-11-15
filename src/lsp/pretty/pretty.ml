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

module Types = struct

  (** Type of constant format strings for effect-full formatters. *)
  type simple = (unit, Format.formatter, unit) format

  (** Type of functions that use a format string with ['a] arguments to return a
      ['b]. *)
  type ('a, 'b) func = ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** Type of procedures that use a format string with ['a] arguments. *)
  type 'a proc = ('a, Format.formatter, unit) format -> 'a

  (** Type of functions that apply an effect-full formatter to pretty-print a
      value of type ['a]. *)
  type 'a printer = Format.formatter -> 'a -> unit

  (** Type of functions that use a formatter to produce some pretty-printed
      result. *)
  type 'a formatting = Format.formatter -> 'a

  (** Type of functions that trigger an application of a fromatter. *)
  type delayed = unit formatting

end

include Types

(* --- *)

(** [print] is a synonym for {!Format.fprintf}. *)
let print: Format.formatter -> 'a proc = Fmt.pf

(** [out] is a synonym for {!Format.printf}. *)
let out: 'a proc = Fmt.pr

(** [error] is a synonym for {!Format.eprintf}. *)
let error: 'a proc = Fmt.epr

(** [delayed] is a synonym for {!Format.dprintf}. *)
let delayed: ('a, delayed) func = Format.dprintf

(** [delayed_to] is a synonym for {!Format.kdprintf}. *)
let delayed_to: (delayed -> 'b) -> ('a, 'b) func = Format.kdprintf

(** Sets right margin to given column (which must be > 2) *)
let pp_set_margin ppf margin =
  Format.pp_set_geometry ppf ~max_indent:(pred margin) ~margin

(** Sends right margin to virtual infinity *)
let blast_margin ppf =   (* see https://github.com/ocaml/ocaml/issues/10592 *)
  pp_set_margin ppf max_int

(** Version of {!Format.asprintf} with virtually no right margin *)
let to_string: ('a, string) func = fun fmt ->
  Fmt.str ("%t@[<h>"^^fmt^^"@]") blast_margin

(** Version of {!Format.kasprintf} with virtually no right margin *)
let string_to: (string -> 'b) -> ('a, 'b) func = fun k fmt ->
  Fmt.kstr k ("%t@[<h>"^^fmt^^"@]") blast_margin

(** Shorhand for {!string_to} {!failwith}: [failwith] raises {!Failure} based on
    the format string [fmt] (possibly with arguments). *)
let failwith fmt =
  string_to failwith fmt

(** [invalid_arg fmt] raises {!Invalid_argument} based on the format string
    [fmt] (possibly with arguments). *)
let invalid_arg fmt =
  string_to (fun s -> raise @@ Invalid_argument s) fmt

(** [styles [s1, ... , sn]] is a shorthand equivalent of [Fmt.styled s1 @@
    Fmt.styled ... @@ Fmt.styled sn]. *)
let styles styles pp =
  List.fold_left (fun pp s -> Fmt.styled s pp) pp styles

(* --- *)

module Simple = struct
  (** Utilities to construct simple format strings from values. *)

  let from_format ?(map = Fun.id) format_string =
    string_to (fun s -> Scanf.format_from_string (map s) "") format_string

  (** [int i] constructs a {!simple} format string as the decimal representation
      of [i]. *)
  let int: int -> simple = from_format "%d"

  (** [char c] constructs a {!simple} format string as the representation of
      [c]. *)
  let char: char -> simple = from_format "%c"

  (** [string s] constructs a {!simple} format string as the representation of
      [s] ({i i.e.}, the resulting format string simply instructs to print the
      given string). *)
  let string: string -> simple = from_format "%s"

  (** [map f format] applies [f] on a string representation of [format], and
      re-constructs a format string from the result. *)
  let map f = from_format ~map:f "@[%(%)@]"

end

(* --- *)

(* Here we add only those we use somewhere in the project (for now --- we could
   at some point also include {!Fmt}) *)
(* include Fmt *)
let char: char printer = Fmt.char
let string: string printer = Fmt.string
let text: string printer = Fmt.text
let int64: int64 printer = Fmt.int64

let option: 'a printer -> 'a option printer = Fmt.option

(* (\** [fmt fmt ppf] is a function that uses [ppf] to pretty-prints arguments *)
(*    according to the format string [fmt].  It is equivalent to [Format.fprintf *)
(*    ppf fmt]. *\) *)
(* let fmt: ('a, Format.formatter, unit) format -> 'a formatting = Fmt.fmt *)

(** [list ?fopen ?fsep ?fclose ?fempty pp_elt ppf list] pretty-prints a list of
    elements using the given simple format strings as delimiters and
    separators. *)
let list
    ?(fopen: simple = "[@[")
    ?(fsep: simple = ",@ ")
    ?(fclose: simple = "@]]")
    ?(fempty: simple option)
  : 'a printer -> 'a list printer
  = fun pp_e ppf lst ->
    let empty =
      List.fold_left begin fun first e ->
        (if first then Fmt.fmt fopen else Fmt.fmt fsep) ppf; pp_e ppf e; false
      end true lst
    in
    if empty
    then match fempty with
      | None -> Fmt.pf ppf "%(%)%(%)" fopen fclose
      | Some fempty -> Fmt.pf ppf fempty
    else Fmt.pf ppf fclose

(** [stack ppf s] pretty-prints a "stack" encoded as a list using a traditional
    layout in some formal-method literature: an empty stack is "[]", otherwise
    the elements are given as a "::"-separated list of elements, starting from
    the top. *)
let stack: 'a printer -> 'a list printer = fun s ->
  list ~fopen:"" ~fsep:"::" ~fclose:"" ~fempty:"[]" s

(** [path ppf strings] uses [ppf] to print the list of strings [strings] as a
    path (typically, colon-separated on Unix-style systems). *)
let path =
  list string ~fopen:"" ~fclose:"" ~fempty:""
    ~fsep:(Simple.char Ez_file.V1.FileOS.path_separator)

let vfield ?(label = Fmt.(styled `Yellow string)) ?(sep = Fmt.any ":@ ")
    l prj pp_v ppf =
  Fmt.pf ppf "@[<v>%a%a%a@]" label l sep () (Fmt.using prj pp_v)

(* --- *)

(** {2 Initialization} *)

(** [straighten_if_tty oc fo ncols] adapts the margins of the formatter [fo],
    whose underlying output channel {i must} be [oc], depending on the kind of
    terminals they it is bound to. *)
let straighten_if_tty oc fo cols =
  if Unix.isatty (Unix.descr_of_out_channel oc) then
    pp_set_margin fo cols

(** Initialization of standard formatters; calls {!Fmt_tty.setup_std_outputs},
    and, depending on the kind of terminals they are bound to, adapts the
    margins of {!Fmt.stdout} and {!Fmt.stderr}.  The width of the terminal
    should be given in number of characters, using the environment variable
    "COLUMNS"; otherwise a width of 180 characters is assumed. *)
let init_formatters ?style_renderer ?utf_8 () =
  Fmt_tty.setup_std_outputs ?style_renderer ?utf_8 ();
  let columns =
    try Option.fold ~none:180 ~some:int_of_string (Sys.getenv_opt "COLUMNS")
    with Invalid_argument _ -> 180
  in
  let columns = if columns < 2 then 180 else columns in
  straighten_if_tty Stdlib.stderr Fmt.stderr columns;
  straighten_if_tty Stdlib.stdout Fmt.stdout columns
