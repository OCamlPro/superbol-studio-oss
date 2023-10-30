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

module Type = Indent_type

(*return the result of indentation. use user-defined indent_config*)
let indent_range = Indenter.indent_range

(** [apply s rdl] applies the changes in [rdl] to the text in [s]. [rdl] is
    expected to have been computed by calling [indent_range ~contents:s]. *)
let apply : string -> Type.indent_record list -> string =
  (*
    [irs] contains a list of {!Type.indent_record} entries describing how to
     modify the indentation.  They must be sorted in increasing order, and there
     must be at most one record per line.

    [buf] is a {!Buffer.t} into which we are writing the indented content.

    [s] is the original content to indent as a {!string}.

    [line] is the current line, which starts at offset [bol] in the string [s].

    [start] is an offset in [s], less than or equal to [bol], that must be
    preserved as-is. This allows copying large unmodified chunks of text (lines
    that are already properly indented) using a single call to
    {!Buffer.add_substring} for reduced overhead.
  *)
  let rec aux ~start (irs : Type.indent_record list) buf s line bol =
    (* Note: this can raise [Invalid_argument] if the modifications do not
       cleanly apply to the input string, which include cases where there are
       modifications on lines past the end of the input, modifications that are
       not sorted, etc. -- we can call [invalid_arg] ourselves, but
       [String.index_from] can also raise [Invalid_argument] in these
       conditions. *)
    match irs with
    | [] when start = 0 -> s
    | [] ->
      Buffer.add_substring buf s start (String.length s - start);
      Buffer.contents buf
    | { lnum; offset_orig; offset_modif } :: irs when line = lnum - 1 ->
      let eol =
        try String.index_from s bol '\n' + 1 with Not_found -> String.length s
      in
      let delta = offset_modif - offset_orig in
      if delta > 0 then (
        Buffer.add_substring buf s start (bol + offset_orig - start);
        for _ = 0 to delta - 1 do
          Buffer.add_char buf ' '
        done;
      ) else (
        Buffer.add_substring buf s start (bol - start + offset_orig + delta);
      );
      aux ~start:(bol + offset_orig) irs buf s (line + 1) eol
    | _ :: _ ->
      match String.index_from s bol '\n' + 1 with
      | eol -> aux ~start irs buf s (line + 1) eol
      | exception Not_found ->
        (* Modifications on lines past the end of the input. *)
        invalid_arg "Cobol_indent.apply"
  in
  fun s rdl ->
    try
      let buf = Buffer.create (String.length s) in
      aux ~start:0 (List.rev rdl) buf s 0 0
    with Invalid_argument _ ->
      Fmt.failwith "Could not indent: internal error"
