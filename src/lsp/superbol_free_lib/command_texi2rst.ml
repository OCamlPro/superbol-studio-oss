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

open EzCompat
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open EzFile.OP
open Cobol_common.Diagnostics (* For Fatal.error *)

module LOCATION = struct
  type t = { filename : string ; line : int }
  let to_string t = Printf.sprintf "%s:%d" t.filename t.line
  let any = { filename = "<generated>"; line = 0 }
end

module INPUT = struct

  type t = {
    ic : in_channel ;
    filename : string ;
    mutable line : int ;
  }

  let open_in filename =
    let ic = open_in filename in
    {
      ic ;
      filename ;
      line = 0;
    }

  let input_line ic =
    let line = input_line ic.ic in
    ic.line <- ic.line + 1;
    line

  let close_in ic = close_in ic.ic

  let error ?ic fmt =
    Printf.kprintf (fun s ->
        begin
          match ic with
          | Some ic ->
              Printf.eprintf "Error at %s:%d: %s\n%!"
                ic.filename ic.line s;
          | None ->
              Printf.eprintf "Error at end of input: %s\n%!" s
        end;
        exit 2
      ) fmt

  let warning ic fmt =
    Printf.kprintf (fun s ->
        Printf.eprintf "Warning at %s:%d: %s\n%!"
                ic.filename ic.line s;
      ) fmt

  let loc ic =
    { LOCATION.filename = ic.filename ; LOCATION.line = ic.line }

end

module OUTPUT = struct

  type t = {
    filename : string ;
    buffer : Buffer.t ;
  }

  let open_out filename =
    let buffer = Buffer.create 10_000 in
    { filename ; buffer }

  let fprintf t fmt = Printf.bprintf t.buffer fmt

  let close_out t =
    EzFile.write_file t.filename (Buffer.contents t.buffer)
end

type line = inline list

and inline =
  | STRING of string
  | MACRO of LOCATION.t * string * line
  | QUOTE of line

type blocks = block list

and block =
    EMPTY_LINE
  | LINE of line
  | NODE of string
  | BLOCK of string * blocks
  | LEVEL of int * string option * line * blocks
  | ITEMS of string * string * ( line * blocks ) list
  | INDEX of string
  | DIAGRAM of string * blocks

type document = {
  basename : string ;
  title : line option ;
  subtitle : line option ;
  authors : line list ;
  content : blocks ;
}

let rec string_of_line line =
  String.concat ""
    (List.map (function
         | STRING s -> s
         | QUOTE arg ->
             Printf.sprintf "@QUOTE[%s]" ( string_of_line arg )
         | MACRO (_loc, macro, arg) ->
             Printf.sprintf "@@%s{{%s}}" macro
               ( string_of_line arg )) line)

let split_command line =
  let len = String.length line in
  if len = 0 then
    line, line
  else
    let rec iter line i len =
      if i = len then
        line, ""
      else
        match line.[i] with
        | ' ' ->
            String.sub line 0 i,
            String.sub line (i+1) (len-i-1)
        | '@'
        | '{' ->
            String.sub line 0 i,
            String.sub line i (len-i)
        | _ ->
            iter line (i+1) len
    in
    iter line 1 len

type block_kind =
    RawBlock
  | Level of int * string option * line
  | Items of string *
             ( line option ) *
             (line * blocks) list

type closer =
  | EOL
  | BRACE
  | QUOTE

exception Unexpected_eol

let read ~path filename =

  let file = Filename.basename filename in
  let dirname = Filename.dirname filename in

  let title = ref None in
  let subtitle = ref None in
  let authors = ref [] in
  let map = ref StringMap.empty in

  let maybe b =
    let s = Buffer.contents b in
    Buffer.clear b;
    if s = "" then [] else [ STRING s ]
  in
  let parse_line_exn ic line =
    let len = String.length line in
    let b = Buffer.create len in
    let rec iter i braced =
      if i = len then
        if braced <> EOL then
          raise Unexpected_eol
        else
          len,
          maybe b
      else
        match line.[i] with
        | '}' ->
          if braced <> BRACE then
            INPUT.error ~ic "unbalanced closing brace" ;
          i+1, maybe b
        | '@' ->
          let i = i+1 in
          if i = len then
            if braced <> EOL then
              INPUT.error ~ic "unexpected end of line"
            else
              len, maybe b
          else
            begin
              let c = line.[i] in
              match c with
              | '{' | '}' | '@' | '.' | '!' | '?' ->
                Buffer.add_char b c ;
                iter (i+1) braced
              | '\n' ->
                iter (i+1) braced
              | '*' ->
                let before = maybe b in
                let i, line = iter (i+1) braced in
                i, before @ MACRO (INPUT.loc ic, "linebreak", []) :: line
              | _ ->
                let before = maybe b in
                let i, line = iter_macro i i braced in
                i, before @ line
            end
        | '`' when i+1 < len && line.[i+1] = '`' ->
          let before = maybe b in
          let i = i+2 in
          let i, quoted = iter i QUOTE in
          let i, line = iter i braced in
          i, before @ QUOTE quoted :: line
        | '\'' when i+1 < len && line.[i+1] = '\'' ->
          if braced <> QUOTE then begin
            (* INPUT.error ~ic "unbalanced ending quote"; *)
            Buffer.add_string b "''";
            iter (i+2) braced
          end else
            let before = maybe b in
            let i = i+2 in
            i, before
        | c ->
          Buffer.add_char b c ;
          iter (i+1) braced

    and iter_macro i pos0 braced =
      if i = len then
        if braced <> EOL then
          INPUT.error ~ic "unexpected end of line"
        else
          let macro = String.sub line pos0 (i-pos0) in
          len, [ MACRO (INPUT.loc ic, macro, []) ]
      else
        match line.[i] with
        | '{' ->
          let macro = String.sub line pos0 (i-pos0) in
          let i, arg = iter (i+1) BRACE in
          let i, line = iter i braced in
          i, begin
            match macro with
            | "value" ->
              let arg = string_of_line arg in
              begin
                match StringMap.find arg !map with
                | s -> STRING s :: line
                | exception Not_found ->
                  INPUT.error ~ic "Unknown variable %S" arg
              end
            | _ -> MACRO (INPUT.loc ic, macro, arg) :: line
          end

        | ' ' | '\t' ->
          let macro = String.sub line pos0 (i-pos0) in
          let i, line = iter (i+1) braced in
          i, MACRO (INPUT.loc ic, macro, []) :: line
        | _ -> iter_macro (i+1) pos0 braced

    in
    let i, line = iter 0 EOL in
    if i<len then
      INPUT.error ~ic "unbalanced ending brace";
    line
  in
  let parse_line ic arg =
    try
      parse_line_exn ic arg
    with
    | Unexpected_eol ->
      INPUT.error ~ic "unexpected end of line"
  in
  let parse_level ic arg =
    let len = String.length arg in
    if len < 3 ||
       arg.[0] <> '{' || arg.[len-1] <> '}' then
      None, parse_line ic arg
    else
      let arg = String.sub arg 1 (len-2) in
      let number, title = EzString.cut_at arg ',' in
      if number = "" || title = "" then
        INPUT.error ~ic "Wrong argument %S for section" arg;
      let title = parse_line ic title in
      Some number, title
  in

  let find_file ?ic file =

    let rec find_in_path path file =
      match path with
      | [] ->
        INPUT.error ?ic "Could not find file %S" file
      | dir :: path ->
        let filename = Filename.concat dir file in
        if Sys.file_exists filename then
          filename
        else
          find_in_path path file
    in
    find_in_path ( dirname :: path) file
  in
  let rec iter_file ?ic file rev stack =
    let filename = find_file ?ic file in
    Printf.eprintf "Reading %S\n%!" filename;
    match INPUT.open_in filename with
    | ic -> iter_lines ic rev stack

  and iter_lines ic rev stack =
    match INPUT.input_line ic with
    | exception _ ->
      INPUT.close_in ic;
      rev, stack
    | line ->
      let command, arg = split_command line in
      match command with

      (* Discard these lines *)
      | "@paragraphindent"
      | "@sp"
      | "@c"
      | "@top"
      | "@settitle"
      | "\\input"
      | "@page"
      | "@unnumbered"
      | "@printindex"
      | "@bye"
      | "@headings"
      | "@oddheading"
      | "@oddfooting"
      | "@evenheading"
      | "@evenfooting"
      | "@validatemenus"
      | "@contents"
      | "@comment"
      | "@comment*"
      | "@setfilename"
      | "@finalout"
      | "@setchapternewpage"
      | "@dircategory"
      | "@*Document"
      | "@*Updates:"
      | "@vskip"
      | "@insertcopying"
      | "@exampleindent"
        ->
        iter_lines ic rev stack

      | "@node" ->
        let arg, _ = EzString.cut_at arg ',' in
        iter_lines ic (NODE arg :: rev) stack
      | "@cindex" ->
        iter_lines ic (INDEX arg :: rev) stack
      | "@set" ->
        let name, value = EzString.cut_at arg ' ' in
        map := StringMap.add name value !map;
        iter_lines ic rev stack
      | "@title" ->
        title := Some ( parse_line ic arg ) ;
        iter_lines ic rev stack
      | "@subtitle" ->
        subtitle := Some ( parse_line ic arg ) ;
        iter_lines ic rev stack ;
      | "@author" ->
        authors := ( parse_line ic arg ) :: !authors ;
        iter_lines ic rev stack
      | "@include" ->
        let rev, stack =
          if arg = "Macros.texi" then rev, stack else
            iter_file ~ic arg rev stack in
        iter_lines ic rev stack

      | "@verbatiminclude" ->
        let lines = EzFile.read_lines_to_list (find_file ~ic arg) in
        let lines = List.map (fun line -> LINE [STRING line]) lines in
        iter_lines ic ( BLOCK("verbatim", lines ) :: rev ) stack

      | "@end" ->
        iter_end ic rev stack arg
      | "@enddict" ->
        iter_end ic rev stack "table"

      | "@float"
      | "@format"
      | "@smallformat"
      | "@cartouche"
      | "@ifhtml"
      | "@html"
      | "@display"
      | "@group"
      | "@raggedright"
      | "@example"
      | "@smallexample"
      | "@ifinfo"
      | "@iftex"
      | "@ifnottex"
      | "@titlepage"
      | "@quotation"
      | "@direntry"
      | "@copying"
      | "@ignore"
      | "@detailmenu"
      | "@menu"
        ->
        let name = String.sub command 1 ( String.length command - 1 ) in
        iter_lines ic [] ( ( name, RawBlock, rev ) :: stack )

      | "@verbatim"
      | "@tex"
        ->
        let name = String.sub command 1 ( String.length command - 1 ) in
        let rec verbatim ic rev =
          match INPUT.input_line ic with
          | exception _ ->
            INPUT.close_in ic;
            INPUT.error ~ic "unclosed verbatim block"
          | "@end verbatim"
          | "@end tex"
            -> List.rev rev
          | line ->
            verbatim ic ( LINE [STRING line] :: rev )
        in
        (* TODO: because we don't interprete these lines, there are @w{} inside. *)
        let verbatim = verbatim ic [] in
        iter_lines ic  ( BLOCK (name, verbatim) :: rev ) stack

      | "@multitable"
      | "@table"
      | "@vtable"
      | "@itemize"
      | "@enumerate"
        ->
        let name = String.sub command 1 ( String.length command - 1 ) in
        iter_lines ic [] ( ( name, Items (arg, None, []), rev ) :: stack )

      | "@headitem" (* TODO: for multitable *)
      | "@item"
      | "@itemx" (* TODO Must improve *)
        (* TODO: ~~~~~~~~~~ in diagrams are removed by rst *)
        ->
        let arg = parse_line ic arg in
        let stack = end_item ic rev stack (Some arg) in
        iter_lines ic [] stack

      | "@chapter"
      | "@newchapter" ->
        iter_section ic rev stack 1 arg
      | "@appendix"
      | "@newappendix" ->
        iter_section ic rev stack 1 arg
      | "@newsection"
      | "@section" ->
        iter_section ic rev stack 2 arg
      | "@subsection"
      | "@newsubsection" ->
        iter_section ic rev stack 3 arg
      | "@newunit"
      | "@subsubsection" ->
        iter_section ic rev stack 4 arg

      | "@diagram" ->
        let len = String.length arg in
        if len < 3 ||
           arg.[0] <> '{' || arg.[len-1] <> '}' then
          INPUT.error ~ic "invalid argument for @diagram";
        let arg = String.sub arg 1 (len-2) in
        begin
          match List.map String.trim @@ EzString.split arg ',' with
          | [ title ; id1 ; id2 ; note ] ->

            if id1 <> id2 then
              INPUT.warning ic "diagram with %s <> %s\n%!"
                id1 id2;

            let lines = EzFile.read_lines_to_list
                ( dirname //
                  Printf.sprintf "SYN-%s.texi" id1) in
            let block =
              DIAGRAM (title, List.map (fun s -> LINE [ STRING s]) lines)
            in
            let note =
              if note = "None" then []
              else

                let rev, _stack =
                  iter_file ~ic (Printf.sprintf "NOTE-%s.texi" note) [] []
                in
                rev
            in
            iter_lines ic ( note @ block :: rev ) stack
          | _ ->
            INPUT.error ~ic "invalid arguments for @diagram";
        end

      | _ ->
        if line = "" then
          iter_lines ic ( EMPTY_LINE :: rev ) stack
        else
          parse_line_rec true ic line rev stack

  and parse_line_rec again ic line rev stack =
    match parse_line_exn ic line with
    | line -> iter_lines ic ( LINE line :: rev ) stack
    | exception Unexpected_eol ->
      if again then
        match INPUT.input_line ic with
        | exception _ ->
          INPUT.close_in ic;
          rev, stack
        | line2 ->
          parse_line_rec false ic (line ^ "\n" ^ line2) rev stack
      else
        INPUT.error ~ic "unexpected end of line after continuation"

  and end_item ic rev stack item_arg =
    match stack with
    | [] ->
      INPUT.error ~ic "@end/@item with empty stack"
    | (name, Items (header, item_arg_before, items), rev_before)
      :: stack_before ->
      let middle =
        match item_arg_before, rev with
        | None, rev ->
          if not ( List.for_all (fun line ->
              match line with
              | EMPTY_LINE -> true
              | _ -> false) rev) then
            let items =
              ( [], List.rev rev ) :: items
            in
            Items (header, item_arg, items)
          else
            Items (header, item_arg, items)
        | Some item_arg_before, _ ->
          let items =
            ( item_arg_before, List.rev rev ) :: items
          in
          Items (header, item_arg, items)
      in
      (name, middle, rev_before) :: stack_before
    | (name, _, _) :: _ ->
      INPUT.error ~ic "@item in %S block" name

  and iter_section ic rev stack level arg =
    let rev, stack = end_section ~ic rev stack level in
    let number, title = parse_level ic arg in
    iter_lines ic [] ( ("section", Level (level, number, title), rev ) :: stack )

  and end_section ?ic rev stack level =
    match stack with
    | [] ->
      if level > 1 then
        INPUT.error ?ic "[sub]section at toplevel";
      rev, stack
    | ("section", Level (level_before, number, title), rev_before ) ::
      stack_before ->
      if level_before >= level then
        let item = LEVEL (level_before, number, title, List.rev rev) in
        let rev = item :: rev_before in
        end_section rev stack_before level
      else
        rev, stack
    | (name, _, _) :: _ ->
      INPUT.error ?ic "missing @end %s\n%!" name

  and iter_end ic rev stack arg =
    let rev, stack =
      match arg with
      | "table"
      | "vtable"
      | "multitable"
      | "enumerate"
      | "itemize" ->
        [], end_item ic rev stack None
      | _ -> rev, stack
    in
    begin
      match stack with
      | [] ->
        INPUT.error ~ic "@end %s with empty stack" arg
      | ( name, content, rev_before ) :: stack_before ->
        if name <> arg then
          INPUT.error ~ic "@end %s but %S expected\n%!"
            arg name ;
        let rev = match name with

          | "iftex"
          | "ifhtml"
          | "titlepage"
          | "direntry"
          | "menu"
            -> rev_before

          | "multitable"
          | "table"
          | "vtable"
          | "enumerate"
          | "itemize"
            ->
            begin
              match content with
              | Items (header, None, items) ->
                ITEMS ( name, header, List.rev items) :: rev_before
              | _ -> assert false
            end

          | _ -> BLOCK ( name, List.rev rev ) :: rev_before
        in
        iter_lines ic rev stack_before

    end

  in
  let rev, stack = iter_file file [] [] in

  let rev, _stack = end_section rev stack 0 in

  {
    basename = Filename.chop_suffix file ".texi";
    content = List.rev rev ;
    title = !title ;
    subtitle = !subtitle ;
    authors = List.rev !authors ;
  }

let spaces = String.make 1000 ' '
let spaces indent = String.sub spaces 0 indent

let print_blocks oc doc =

  let rec iter indent doc =
    match doc with
    | [] -> ()
    | item :: doc ->
        begin
          match item with
          | EMPTY_LINE -> Printf.fprintf oc "%sEMPTY_LINE\n" (spaces indent)
          | NODE index ->
              Printf.fprintf oc "%sNODE %s\n" (spaces indent) index
          | INDEX index ->
              Printf.fprintf oc "%sINDEX %s\n" (spaces indent) index
          | LINE line ->
              Printf.fprintf oc "%sLINE[%s]\n"
                ( spaces indent ) ( string_of_line line )
          | BLOCK (name, doc) ->
              Printf.fprintf oc "%sBEGIN %s <<<<<\n" ( spaces indent ) name ;
              iter ( indent + 2 ) doc;
              Printf.fprintf oc "%sEND %s >>>>>\n" ( spaces indent ) name;
          | DIAGRAM (name, doc) ->
              Printf.fprintf oc "%sDIAGRAM %s <<<<<\n" ( spaces indent ) name ;
              iter ( indent + 2 ) doc;
              Printf.fprintf oc "%sEND %s >>>>>\n" ( spaces indent ) name;
          | LEVEL (level, number, title, doc) ->
              Printf.fprintf oc "%sLEVEL %d %s -> %s <<<<<\n"
                ( spaces indent ) level
                ( match number with
                  | None -> "DIRECT"
                  | Some number -> number) (string_of_line title ) ;
              iter ( indent + 2 ) doc;
              Printf.fprintf oc "%sEND LEVEL %d >>>>>\n" ( spaces indent ) level
          | ITEMS (name, header, items) ->
              Printf.fprintf oc "%sITEMS %s %s <<<<<\n"
                ( spaces indent ) name header;
              List.iter (fun (title, item) ->
                  Printf.fprintf oc "%s  ITEM %s\n" ( spaces indent )
                    ( string_of_line title );
                  iter (indent + 4) item
                ) items ;
              Printf.fprintf oc "%sEND ITEMS %s >>>>>\n" ( spaces indent ) name
        end;
        iter indent doc

  in
  iter 0 doc

let print_doc ?dir doc =
  let oc = match dir with
    | None -> stdout
    | Some dir -> open_out ( dir // doc.basename ^ ".format" )
  in
  begin
    match doc.title with
    | None -> ()
    | Some title -> Printf.fprintf oc "TITLE: %S\n%!" ( string_of_line title )
  end;
  begin match doc.subtitle with
    | None -> ()
    | Some title -> Printf.fprintf oc "SUBTITLE: %S\n%!"
                      ( string_of_line title )
  end;
  List.iter (fun author ->
      Printf.fprintf oc "AUTHOR: %S\n%!"
        ( string_of_line author ) ) doc.authors;
  print_blocks oc doc.content ;
  Printf.fprintf oc "@!";
  match dir with
  | None -> ()
  | Some _ -> close_out oc

let label_of_title s =
  String.map (fun c ->
      match c with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> c
      | _ -> 'A') s

type verbatim =
  | Block
  | LiteralBlock
  | ParsedLiteral

type ctx = {
  doc : document ;
  mutable math : bool list;
  mutable verbatim : verbatim list ;

  mutable line : int ;
  line_indexes : (int, string) Hashtbl.t;
  new_line_indexes : (int, string) Hashtbl.t;
  line_anchors : (int, string) Hashtbl.t;
  new_line_anchors : (int, string) Hashtbl.t;

  refs_external : ( string * string ) StringMap.t ;
  refs_allowed : StringSet.t option ;
  mutable files : string list ;
  mutable footnotes : string list ;
  mutable unknown_macros : LOCATION.t StringMap.t ;
  mutable refs_created : StringSet.t ;
  mutable anchors_created : StringSet.t ;
  mutable index_created : StringSet.t ;
  mutable refs_used : LOCATION.t StringMap.t ;
}

let add_index ctx label =
  ctx.refs_created <- StringSet.add label ctx.refs_created ;
  Hashtbl.add ctx.new_line_indexes ctx.line label

let add_anchor ctx label =
  let label = label_of_title label in
  ctx.refs_created <- StringSet.add label ctx.refs_created ;
  ctx.anchors_created <- StringSet.add label ctx.anchors_created ;
  Hashtbl.add ctx.new_line_anchors ctx.line label

let place_for_indexes ctx oc indent =
  ctx.line <- ctx.line + 1;

  let indexes = Hashtbl.find_all ctx.line_indexes ctx.line in
  if indexes <> [] then begin
    OUTPUT.fprintf oc "\n";
    List.iter (fun label ->
        OUTPUT.fprintf oc "%s.. index:: single:%s\n" indent label;
      ) indexes;
    OUTPUT.fprintf oc "\n";
  end;
  let anchors = Hashtbl.find_all ctx.line_anchors ctx.line in
  if anchors <> [] then begin
    OUTPUT.fprintf oc "\n";
    List.iter (fun label ->
        OUTPUT.fprintf oc "\n%s.. _%s:\n" indent label;
      ) anchors;
    OUTPUT.fprintf oc "\n";
  end;
  ()

let rst_escape s =
  let len = String.length s in

  let rec escape b s i len =
    if i = len then
      Buffer.contents b
    else
      let c = s.[i] in
      begin
        match c with
        | '*' | '`' -> Buffer.add_char b '\\'; Buffer.add_char b c
        | _ -> Buffer.add_char b c
      end;
      escape b s (i+1) len
  in
  let rec iter s i len =
    if i = len then
      s
    else
      match s.[i] with
      | '*' | '`' ->
          let b = Buffer.create (len+10) in
          escape b s 0 len
      | _ -> iter s (i+1) len
  in
  iter s 0 len

let rst_trim s =
  let len = String.length s in
  let rec iter s i len =
    if i<len && s.[i] = ' ' then
      iter s (i+1) len
    else
      i
  in
  let begin_s = iter s 0 len in
  if begin_s = len then ""
  else
    let rec iter s i len =
      if i>0 && s.[i-1] = ' ' && (i=1 || s.[i-2] <> '\\') then
        iter s (i-1) len
      else
        i
    in
    let end_s = iter s len len in
    if begin_s = 0 && end_s = len then s else
      String.sub s begin_s (end_s - begin_s)

let rec rst_of_line ctx line =
  String.concat "" @@
  List.map (function
      | STRING s ->
          begin
            match ctx.math, ctx.verbatim with
            | true :: _, _
            | _, LiteralBlock :: _ -> s
            | _ ->
                rst_escape s
          end
      | QUOTE q -> Printf.sprintf "\"%s\"" ( rst_of_line ctx q )

      | MACRO ( loc, name, arg) ->
          match name with

          | "_" -> rst_of_line ctx arg

          (******                               texinfo generic macros *)

          | "`" -> if List.hd ctx.verbatim = LiteralBlock then "`" else "\\`"
          | "TeX" -> "TeX"
          | "w" -> ""
          | "noindent" -> ""
          | "anchor" ->
              let arg = rst_of_line0 ctx arg in
              add_anchor ctx arg;
              ""
          | "i"
            ->
              let arg = rst_of_line0 ctx arg in
              Printf.sprintf "\\ *%s*\\ " arg
          | "dfn"
            ->
              let arg = rst_of_line0 ctx arg in
              Printf.sprintf "\\ *%s*\\ " arg
          | "kbd"
          | "option"
          | "env" -> rst_of_line ctx [ MACRO (loc, "code", arg)]
          | "sc" -> rst_of_line ctx [ MACRO (loc, "small-caps", arg)]
          | "code" ->
              let arg =
                let verbatim_stack = ctx.verbatim in
                ctx.verbatim <- LiteralBlock :: ctx.verbatim;
                let arg = rst_of_line ctx arg in
                ctx.verbatim <- verbatim_stack ;
                arg
              in
              if List.hd ctx.verbatim = LiteralBlock then
                arg
              else
                let arg = rst_trim arg in
                Printf.sprintf "\\ :%s:`%s`\\ " name arg
          | "small-caps"
          | "command"
          | "file"
            ->
              let arg = rst_of_line ctx arg in
              if List.hd ctx.verbatim = LiteralBlock then
                arg
              else
                Printf.sprintf " :%s:`%s`" name arg
          | "math" ->
              let math_stack = ctx.math in
              ctx.math <- true :: math_stack ;
              let arg = rst_of_line ctx arg in
              ctx.math <- math_stack ;
              Printf.sprintf " :math:`%s`" arg
          | "email" -> rst_of_line ctx arg
          | "uref"
          | "url"
             ->
              let arg = rst_of_line ctx arg in
              let url, name = EzString.cut_at arg ',' in
              if name <> "" then
                Printf.sprintf " `%s <%s>`_" (rst_trim name) url
              else
                Printf.sprintf " `<%s>`_" url
          | "var" ->
              let arg = rst_of_line0 ctx arg in
              Printf.sprintf "<%s>" arg
          | "emph"
          | "strong"
          | "b"
            ->
              let arg = rst_of_line0 ctx arg in
              Printf.sprintf "\\ **%s**\\ " arg
          | "ref" ->
              let arg = rst_of_line0 ctx arg in
              add_ref ctx loc arg
          | "pxref"
          | "xref"
            ->
              let arg = parse_args ctx arg in
              begin
                match arg with
                | arg :: _ ->
                    add_ref ctx loc arg
                | [] -> assert false
              end
          | "acronym" ->
              let arg = rst_of_line0 ctx arg in
              Printf.sprintf ":abbr:`%s`" arg
          | "linebreak" -> assert false
          | "dots" -> "..."
          | "center" -> "\n               " (* TODO improve *)
          | "footnote" ->
              let arg = rst_of_line ctx arg in
              ctx.footnotes <- arg :: ctx.footnotes;
              " [#]_ "
          | "leq" -> " :math:`\\leq`"
          | "r" ->
              let arg = rst_of_line ctx arg in
              if List.hd ctx.verbatim = LiteralBlock then
                arg
              else
                arg
          | "" ->
              assert (arg = []);
              if List.hd ctx.math then "~" else " |_| "
          | "tab" | "result" ->
              assert (arg = []);
              " "

          (* gnucobol specific macros from Macros.texi *)


          | "anchoridx" ->
              rst_of_line ctx [ MACRO (loc, "idx", arg);
                                MACRO (loc, "anchor", arg) ]
          | "define"
          | "itemdfn"
            ->
              rst_of_line ctx [ MACRO (loc, "idx", arg ) ;
                                MACRO (loc, "dfn", arg ) ]
          | "directive" ->
            rst_of_line ctx [ MACRO (loc, "code", arg) ;
                              STRING " CDF directive" ]
          | "directiveref" ->
              with_pxref ctx loc "directive" arg
          | "envvarcompile" ->
              with_pxref ctx loc "code" ~arg
                ~prefix: [
                  MACRO (loc, "idx", arg @ [ STRING " Environment Variable"]) ;
                  MACRO (loc, "idx", [ STRING " Environment Variables, "] @ arg) ;
                ]
                ~suffix:" compilation-time environment variable"
                [ STRING "Compilation Time Environment Variables" ]
          | "envvarruntime" ->
              rst_of_line ctx [
                MACRO (loc, "idx", arg @ [ STRING " Environment Variable"]) ;
                MACRO (loc, "idx", [ STRING " Environment Variables, "] @ arg) ;
                STRING " run-time environment variable" ]
          | "envvarruntimeref" ->
              with_pxref ctx loc "envvarruntime" ~arg
                [ STRING "Run Time Environment Variables" ]
          | "envvarruntimerefs" ->
              with_pxref ctx loc "envvarruntime" ~arg
                ~suffix: " run-time environment variables"
                [ STRING "Run Time Environment Variables" ]
          | "idx" ->
              let arg = rst_of_line0 ctx arg in
              add_index ctx arg;
              if List.hd ctx.verbatim = Block then "\\ " else ""
          | "intrinsic" ->
              rst_of_line ctx [ MACRO (loc, "code", arg) ; STRING " intrinsic function" ]
          | "intrinsicref" ->
              with_pxref ctx loc "intrinsic" arg
          (* newappendix *)
          (* newappsec *)
          (* newchapter *)
          (* newsection *)
          (* newsubsection *)
          (* newunit *)
          | "registertext" ->
              rst_of_line ctx [ MACRO (loc, "code", arg) ; STRING " special register" ]
          | "register" ->
              rst_of_line ctx [ MACRO (loc, "idx", arg @ [ STRING " Special Register" ] );
                                MACRO (loc, "idx", [ STRING " Special Registers, " ] @ arg  );
                                MACRO (loc, "registertext", arg);
                              ]
          | "registerref" ->
              with_pxref ctx loc "register" ~arg [ STRING "Special Registers" ]
          | "registerrefalt" ->
              with_pxrefalt ctx loc "register" arg
          | "statement" ->
              rst_of_line ctx [ MACRO (loc, "code", arg) ; STRING " statement" ]
          | "statementref" ->
              with_pxref ctx loc "statement" arg
          | "statementrefalt" ->
              with_pxrefalt ctx loc "statement" arg
          | "subpgm" ->
              rst_of_line ctx [ MACRO (loc, "code", arg) ; STRING " built-in system subroutine" ]
          | "subpgmref" ->
              with_pxref ctx loc "subpgm" arg
          | "switch" ->
              rst_of_line ctx [ MACRO (loc, "option", arg) ; STRING " switch" ]
          | "switchidx" ->
              rst_of_line ctx [ MACRO (loc, "idx", [ STRING "Compiler Switches, " ] @ arg) ;
                                MACRO (loc, "idx", arg @ [ STRING " Compiler Switch" ]) ;
                                MACRO (loc, "switch", arg) ]
          | "syntaxidx" ->
              rst_of_line ctx [ MACRO (loc, "idx", arg);
                                MACRO (loc, "code", arg); ]
          | "plainidx" ->
              rst_of_line ctx ( [ MACRO (loc, "idx", arg) ] @ arg )
          | "syntaxref" ->
              with_pxref ctx loc "code" arg
          | "syntaxrefalt" ->
              with_pxrefalt ctx loc "code" arg
          | "termrefalt" ->
              with_pxrefalt ctx loc "i" arg
          | "topic" ->
              with_pxref ctx loc "_" arg


          | "t" -> rst_of_line ctx [ MACRO (loc, "code", arg) ]
          | "key"
            ->
              let arg = rst_of_line0 ctx arg in
              Printf.sprintf "\\ :code:`%s`\\ " arg
          | "samp" ->
              let arg = rst_of_line0 ctx arg in
              if List.hd ctx.verbatim = LiteralBlock then
                Printf.sprintf "'%s'" arg
              else
                Printf.sprintf "'\\ :code:`%s`\\ '" arg

          | "sup" ->
              let arg = rst_of_line ctx arg in
              if List.hd ctx.math then
                Printf.sprintf "^{%s}" arg
              else
                Printf.sprintf "\\ :sup:`%s`\\ " arg

          | "copyright" -> "\\ |copy|\\ "
          | command ->
              let arg = rst_of_line ctx arg in
              if not ( StringMap.mem command ctx.unknown_macros ) then begin
                Printf.eprintf "%s: unknown macro %S\n%!"
                  (LOCATION.to_string loc) command ;
                ctx.unknown_macros <- StringMap.add command loc ctx.unknown_macros;
              end ;
              Printf.sprintf "@%s{{ %s }}" command arg
    ) line

and add_ref ctx loc arg =
  let label = label_of_title arg in
  match StringMap.find label ctx.refs_external with
  | (anchor, ref) ->
      Printf.sprintf " `%s <%s>`_" ref anchor
  | exception Not_found ->
      ctx.refs_used <- StringMap.add label loc ctx.refs_used;
      if StringSet.mem label ctx.anchors_created then
        Printf.sprintf " :ref:`%s <%s>`" arg label
      else
        Printf.sprintf " :ref:`%s`" label

and parse_args ctx arg =
  let arg = rst_of_line ctx arg in
  List.map rst_trim (EzString.split arg ',')

and rst_of_line0 ctx line =
  let arg = rst_of_line ctx line in
  if List.hd ctx.verbatim = LiteralBlock then
    arg
  else
    rst_trim arg

and with_pxref ctx loc name ?arg ?(prefix=[]) ?suffix ref =
  let arg = match arg with
    | None -> ref
    | Some arg -> arg
  in
  let prefix = match suffix with
    | None ->  prefix @ [ MACRO (loc, name, arg) ]
    | Some suffix -> prefix @ [ MACRO (loc, name, arg); STRING suffix ]
  in
  rst_of_line ctx (prefix @ [ STRING " ("; MACRO (loc, "pxref", ref); STRING ")" ])

and with_pxrefalt ctx loc name arg =
  match parse_args ctx arg with
  | [ text ; ref ] ->
      with_pxref ctx loc name ~arg:[ STRING text ] [ STRING ref ]
  | _ -> assert false

let rst_of_line = rst_of_line0

let output_level ctx oc level ?number title =

  let title = rst_of_line ctx title in
  add_index ctx title ;

  OUTPUT.fprintf oc "\n";
  if match number with
    | None -> false
    | Some s ->
        match s.[0] with
        | '0'..'9' -> true
        | _ -> false
  then begin
    let label = label_of_title title in
    ctx.refs_created <- StringSet.add label ctx.refs_created;
    OUTPUT.fprintf oc "\n.. _%s:\n" label;
  end;
  let title = match number with
    | None -> title
    | Some number -> Printf.sprintf "%s %s" number title
  in

  let len = String.length title in
  let c = match level with
    | 1 -> '='
    | 2 -> '-'
    | 3 -> '~'
    | 4 -> '^'
    | _ -> assert false
  in
  OUTPUT.fprintf oc "\n%s\n" title ;
  OUTPUT.fprintf oc "%s\n" ( String.make len c)

let linebreaks line =
  if List.exists (function
        MACRO (_loc, "linebreak", []) -> true
      | _ -> false ) line then
    let rec iter lines rev line =
      match line with
      | [] ->
          List.rev
            ( match rev with
              | [] ->  lines
              | _ -> List.rev rev :: lines )
      | MACRO ( _loc, "linebreak", [] ) :: line ->
          iter (List.rev rev :: lines ) [] line
      | inline :: line ->
          iter lines ( inline :: rev ) line
    in
    iter [] [] line
  else
    [ line ]

let rec output_blocks ctx oc indent blocks =
  match blocks with
  | [] -> ()
  | [ EMPTY_LINE ] -> ()
  | block :: blocks ->
      output_block ctx oc indent block;
      output_blocks ctx oc indent blocks

and output_block ctx oc indent block =
  match block with
  | LINE line ->
      begin
        match linebreaks line with
        | [ line ] ->
            let line = rst_of_line ctx line in
            begin
              match line with
                "\\" | "\\ " -> ()
              | _ -> OUTPUT.fprintf oc "%s%s\n" indent line
            end
        | lines ->
            List.iter (fun line ->
                OUTPUT.fprintf oc "%s| %s\n" indent
                  ( rst_of_line ctx line )
              ) lines
      end
  | INDEX index ->
      add_index ctx index
  | NODE index ->
      add_anchor ctx index
  | EMPTY_LINE ->
      OUTPUT.fprintf oc "%s\n" indent ;
      place_for_indexes ctx oc indent ;
      ()
  | LEVEL (level, number, title, blocks) ->
      assert (indent = "");
      place_for_indexes ctx oc indent ;
      output_level ctx oc level ?number title ;
      place_for_indexes ctx oc indent ;
      output_blocks ctx oc "" blocks ;

  | BLOCK ("quotation", blocks) ->
      OUTPUT.fprintf oc "\n%s\n%s" indent indent;
      output_blocks ctx oc (indent^"  ") blocks;
      OUTPUT.fprintf oc "%s\n" indent

  | BLOCK ("group", blocks) ->
      output_blocks ctx oc indent blocks

  | DIAGRAM (title, blocks) ->
      if ctx.doc.basename = "gnucobqr" then begin
        place_for_indexes ctx oc "" ;
        output_level ctx oc 2 [ STRING ( title ^ " Syntax") ] ;
      end
      else
        OUTPUT.fprintf oc "%s%s Syntax\n" indent title;

      let verbatim_stack = ctx.verbatim in
      ctx.verbatim <- LiteralBlock :: verbatim_stack ;
      begin
        match List.hd verbatim_stack with
        | Block ->
            OUTPUT.fprintf oc "%s::\n  \n" indent;
            output_blocks ctx oc (indent^"  ") blocks;
            OUTPUT.fprintf oc "%s\n" indent;
        | ParsedLiteral | LiteralBlock ->
            output_blocks ctx oc indent blocks
      end;
      ctx.verbatim <- verbatim_stack

  | BLOCK ("verbatim", blocks)
  | BLOCK ("example", blocks)
  | BLOCK ("smallexample", blocks)
  | BLOCK ("format", blocks)
  | BLOCK ("smallformat", blocks)
    ->
      let verbatim_stack = ctx.verbatim in
      ctx.verbatim <- LiteralBlock :: verbatim_stack ;
      begin
        match List.hd verbatim_stack with
        | Block ->
            OUTPUT.fprintf oc "%s::\n  \n" indent;
            output_blocks ctx oc (indent^"  ") blocks;
            OUTPUT.fprintf oc "%s\n" indent;
        | ParsedLiteral | LiteralBlock ->
            output_blocks ctx oc indent blocks
      end;
      ctx.verbatim <- verbatim_stack

  | BLOCK ("display", blocks)
    ->
      let verbatim_stack = ctx.verbatim in
      begin
        match List.hd verbatim_stack with
        | ParsedLiteral | LiteralBlock ->
            output_blocks ctx oc indent blocks
        | Block ->
            ctx.verbatim <- LiteralBlock :: verbatim_stack ;
            OUTPUT.fprintf oc "%s\n%s.. parsed-literal::\n  \n" indent indent;
            output_blocks ctx oc (indent^"  ") blocks;
            OUTPUT.fprintf oc "%s\n" indent;
            ctx.verbatim <- verbatim_stack
      end;
  | BLOCK ( ("ifnottex" | "ifinfo" | "ignore"), _ ) -> ()
  | BLOCK (name, blocks) -> (* TODO *)

      begin
        match name with
        | "cartouche" -> ()
        | _ ->
            OUTPUT.fprintf oc
              "%s\nERROR: uninterpreted block %S\n\n" indent name;
      end;

      output_blocks ctx oc indent blocks
  | ITEMS (name, header, lines)->

      match name with

      | "table" ->

          let in_indent = indent ^ "  " in
          List.iteri (fun i (title, blocks) ->

              place_for_indexes ctx oc (if i = 0 then indent else in_indent) ;

              let style = String.sub header 1 ( String.length header - 1) in
              let title =
                if style = "asis" then
                  title
                else
                  [ MACRO (LOCATION.any, style, title)]
              in
              OUTPUT.fprintf oc "\n\n%s* %s\n\n" indent
                (rst_of_line ctx title);
              output_blocks ctx oc in_indent blocks;
              OUTPUT.fprintf oc "%s\n" in_indent;

            ) lines

      | "enumerate" ->

          let in_indent = indent ^ "   " in
          List.iteri (fun i (title, blocks) ->
              place_for_indexes ctx oc (if i = 0 then indent else in_indent) ;
              OUTPUT.fprintf oc "\n\n%s#. %s\n\n" indent
                (rst_of_line ctx title);
              output_blocks ctx oc in_indent blocks;
              OUTPUT.fprintf oc "%s\n" in_indent;
            ) lines

      | "itemize"
      | _ ->

          let in_indent = indent ^ "  " in
          List.iteri (fun i (title, blocks) ->
              place_for_indexes ctx oc (if i = 0 then indent else in_indent) ;
              OUTPUT.fprintf oc "\n\n%s* %s\n\n" indent
                (rst_of_line ctx title);
              output_blocks ctx oc in_indent blocks;
              OUTPUT.fprintf oc "%s\n" indent;
            ) lines

let to_rst doc dir =

  let refs_external =
    let anchors_file = doc.basename ^ ".anchors" in
    let map = ref StringMap.empty in
    if Sys.file_exists anchors_file then begin
      let lines = EzFile.lines_of_file anchors_file in
      Array.iter (fun line ->
          let ref, line = EzString.cut_at line ' ' in
          let anchor, text = EzString.cut_at line ' ' in
          map := StringMap.add ref (anchor, text) !map;
        ) lines
    end;
    !map
  in
  let pass ~gen_files
      ?(line_indexes = Hashtbl.create 1000)
      ?(line_anchors = Hashtbl.create 1000)
      ?(anchors_created = StringSet.empty)
      () =
    let ctx = {
      doc = doc ;
      math = [ false ] ;
      verbatim = [ Block ] ;
      refs_allowed = None ;
      refs_external ;
      line_indexes ;
      line_anchors ;
      line = 0;

      files = [] ;
      footnotes = [] ;
      unknown_macros = StringMap.empty ;
      refs_created = StringSet.empty ;
      anchors_created ;
      index_created = StringSet.empty ;
      refs_used = StringMap.empty ;
      new_line_indexes = Hashtbl.create 1000;
      new_line_anchors = Hashtbl.create 1000;
    }
    in

    let chapters = ref 0 in

    let rst_header = {|
.. |_| unicode:: 0xA0
   :trim:

.. role:: small-caps
   :class: small-caps

.. include:: <isonum.txt>

|}
    in
    List.iter (fun block ->

        match block with
        | BLOCK ( "copying", blocks ) ->
          let file = "copying.rst" in
          ctx.files <- file :: ctx.files ;
          let oc = OUTPUT.open_out ( dir // file ) in

          OUTPUT.fprintf oc "%s" rst_header;
          place_for_indexes ctx oc "" ;
          output_level ctx oc 1 [ STRING "Copyright" ] ;
          output_blocks ctx oc "" blocks ;

          List.iter (fun arg ->
              OUTPUT.fprintf oc "\n\n.. [#] %s\n" arg;
            ) ( List.rev ctx.footnotes );

          if gen_files then OUTPUT.close_out oc


        | LEVEL (1, number, title, blocks) ->
          incr chapters ;
          let file = Printf.sprintf "chapter%d.rst" !chapters in
          ctx.files <- file :: ctx.files ;
          ctx.footnotes <- [] ;
          let oc = OUTPUT.open_out ( dir // file ) in

          OUTPUT.fprintf oc "%s" rst_header;
          place_for_indexes ctx oc "" ;
          output_level ctx oc 1 ?number title ;
          output_blocks ctx oc "" blocks ;

          List.iter (fun arg ->
              OUTPUT.fprintf oc "\n\n.. [#] %s\n" arg;
            ) ( List.rev ctx.footnotes );

          if gen_files then OUTPUT.close_out oc
        | BLOCK ("ifnottex", _ ) -> ()
        | BLOCK ("ifinfo", _ ) -> ()
        | LINE line ->
          Printf.eprintf "Discarding toplevel line %s\n%!"
            ( string_of_line line )
        | EMPTY_LINE -> ()
        | NODE _ -> ()
        | _ ->
          Printf.eprintf "ERROR:<<<\n%!";
          print_blocks stderr [block];
          Printf.eprintf ">>>\n%!";
          assert false
      ) doc.content ;


    ctx.files <- List.rev ctx.files ;
    ctx
  in

  let ctx = pass ~gen_files:false () in
  let ctx = pass
      ~gen_files:true
      ~line_indexes:ctx.new_line_indexes
      ~line_anchors:ctx.new_line_anchors
      ~anchors_created:ctx.anchors_created
      () in
  let files = ctx.files in

  let oc = OUTPUT.open_out ( dir // "index.rst" ) in
  let title = string_of_line ( match doc.title with
      | None -> assert false
      | Some title -> title )
  in

  OUTPUT.fprintf oc "%s\n"
    ( String.concat "\n"
        (
          [ ".. gnucobol documentation master file";
            "";
            title;
            String.make ( String.length title ) '=' ;
            "";
            "This documentation is published by OCamlPro SAS on our `resources page for GnuCOBOL <https://cobolix.com/gnucobol>`_.";
            "" ;
            "Authors:" ;
            "";
          ] @
          List.map (fun author ->
              Printf.sprintf "* %s" @@ rst_of_line ctx author) doc.authors
          @ [
            "";
            ".. toctree::";
            "   :maxdepth: 2";
            "   :caption: Documentation";
            "";
          ]));
  List.iter (fun file ->
      OUTPUT.fprintf oc "   %s\n" ( Filename.chop_suffix file ".rst"))
    files;

  OUTPUT.fprintf oc "%s\n"
    ( String.concat "\n"
        [
          "";
          "Indices and tables";
          "==================";
          "";
          "* :ref:`genindex`";
          "* :ref:`modindex`";
          ""
        ]);
  OUTPUT.close_out oc;

  StringMap.iter (fun label loc ->
      if not ( StringSet.mem label ctx.refs_created ) then
        Printf.eprintf "%s: undefined label %S\n%!"
          ( LOCATION.to_string loc ) label
    ) ctx.refs_used ;

  ()

let action ~path ~filename ?target () =

  let doc = read ~path filename in
  match target with
  | None ->
      print_doc doc
  | Some dir ->
      print_doc doc ~dir ;
      to_rst doc dir


let cmd =
  let filename = ref None in
  let target = ref None in
  let path = ref [] in
  EZCMD.sub
    "texi2rst"
    (fun () ->
       let path = List.rev !path in
       match !filename with
       | None -> Fatal.error "You must specify a filename"
       | Some filename -> action ~path ~filename ?target:!target ()
    )
    ~args:
      [
        [ "o" ], Arg.String (fun s -> target := Some s),
        EZCMD.info ~docv:"DIR" "Target directory for RST generation";

        [ "I" ], Arg.String (fun s -> path := s :: !path),
        EZCMD.info ~docv:"DIR" "Add to lookup path for files";

        [],
        Arg.Anon (0, fun s -> filename := Some s),
        EZCMD.info ~docv:"FILE" ".texi file"
      ]
    ~doc:
      "build .texi documentation from gnucobol-docs"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Build .texi documentation from gnucobol-docs."
      ];
    ]
