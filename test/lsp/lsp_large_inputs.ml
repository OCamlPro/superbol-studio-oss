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

(* Regression tests for stack overflows on very large inputs.  They are mostly
   relevant for the server compiled into JavaScript (see `js` sub-directory),
   where the call stack is much smaller than with native code. *)

open Lsp.Types
open Lsp_testing

let size = 20_000

let ( @ ) a b = List.rev_append (List.rev a) b             (* tail-recursive *)

(** [gen_program ~data ~procedure] generates a program with [data] lines in its
    working-storage section, followed by [procedure] lines in its procedure
    division.  Data items [V0] to [V9] are always defined.  Returns the program
    along with the position of the first occurrence of [V1] in its last
    procedure line, if any. *)
let gen_program ?(data = []) procedure =
  let buf = Buffer.create (size * 40) in
  let line = ref 0 in
  let add s = Buffer.add_string buf ("       " ^ s ^ "\n"); incr line in
  let pos = ref None in
  add "IDENTIFICATION DIVISION.";
  add "PROGRAM-ID. prog.";
  add "DATA DIVISION.";
  add "WORKING-STORAGE SECTION.";
  for i = 0 to 9 do add (Printf.sprintf "01 V%d PIC 9(4)." i) done;
  List.iter add data;
  add "PROCEDURE DIVISION.";
  List.iter begin fun s ->
    (match Str.search_forward (Str.regexp_string "V1") s 0 with
     | i -> pos := Some (Position.create ~line:!line ~character:(i + 7))
     | exception Not_found -> ());
    add s
  end procedure;
  add "    STOP RUN.";
  Buffer.contents buf, !pos

let rec count_symbols (symbols: DocumentSymbol.t list) =
  List.fold_left begin fun acc (s: DocumentSymbol.t) ->
    acc + 1 + count_symbols (Option.value s.children ~default:[])
  end 0 symbols

let check (prog, pos) =
  let { projdir; end_with_postproc }, server = make_lsp_project () in
  let server, textDocument = add_cobol_doc server ~projdir "prog.cob" prog in
  let open LSP.Request.INTERNAL in
  let run name f =
    match f () with
    | s -> Pretty.out "%s: %s@." name s
    | exception Stack_overflow -> Pretty.out "%s: Stack overflow@." name
  in
  run "semantic tokens" begin fun () ->
    match semtoks_full server (SemanticTokensParams.create ~textDocument ()) with
    | None -> "none"
    | Some { data; _ } -> string_of_int (Array.length data / 5)
  end;
  run "document symbols" begin fun () ->
    match document_symbol server
            (DocumentSymbolParams.create ~textDocument ()) with
    | None -> "none"
    | Some `DocumentSymbol l -> string_of_int (count_symbols l)
  end;
  run "code lenses" begin fun () ->
    string_of_int @@ List.length @@
    codelens server (CodeLensParams.create ~textDocument ())
  end;
  run "formatting edits" begin fun () ->
    let options = FormattingOptions.create ~insertSpaces:true ~tabSize:2 () in
    match formatting server
            (DocumentFormattingParams.create ~options ~textDocument ()) with
    | None -> "none"
    | Some l -> string_of_int (List.length l)
  end;
  Option.iter begin fun position ->
    run "hover" begin fun () ->
      match hover server (HoverParams.create ~position ~textDocument ()) with
      | None -> "none"
      | Some _ -> "some"
    end;
    run "definitions" begin fun () ->
      match lookup_definition server
              (DefinitionParams.create ~position ~textDocument ()) with
      | None -> "none"
      | Some `Location l -> string_of_int (List.length l)
    end;
    run "references" begin fun () ->
      let context = ReferenceContext.create ~includeDeclaration:true in
      match lookup_references server
              (ReferenceParams.create ~position ~textDocument ~context ()) with
      | None -> "none"
      | Some l -> string_of_int (List.length l)
    end;
    run "rename edits" begin fun () ->
      let WorkspaceEdit.{ changes; documentChanges; _ } =
        rename server
          (RenameParams.create ~newName:"W" ~position ~textDocument ()) in
      ignore documentChanges;
      string_of_int @@ List.fold_left (fun n (_, l) -> n + List.length l) 0 @@
      Option.value changes ~default:[]
    end;
    run "completion items" begin fun () ->
      match completion server
              (CompletionParams.create ~position ~textDocument ()) with
      | None -> "none"
      | Some `CompletionList { items; _ } -> string_of_int (List.length items)
    end;
  end pos;
  end_with_postproc

let%expect_test "many-data-items" =
  let end_with_postproc =
    check @@ gen_program [ "    MOVE 1 TO V1." ]
      ~data:(List.init size (Printf.sprintf "01 W%d PIC X."))
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    semantic tokens: 100074
    document symbols: 20015
    code lenses: 20010
    formatting edits: 0
    hover: some
    definitions: 1
    references: 2
    rename edits: 2
    completion items: 20019 |}]

let%expect_test "long-sentence" =
  let end_with_postproc =
    check @@ gen_program @@
    List.init size (fun i -> Printf.sprintf "    MOVE %d TO V%d" i (i mod 10)) @
    [ "    MOVE 1 TO V1." ]
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    semantic tokens: 80074
    document symbols: 15
    code lenses: 10
    formatting edits: 0
    hover: some
    definitions: 1
    references: 2002
    rename edits: 2002
    completion items: 19 |}]

let%expect_test "many-paragraphs" =
  let end_with_postproc =
    check @@ gen_program @@
    List.init size (Printf.sprintf "    PERFORM P%d.") @
    List.concat_map (fun i -> [ Printf.sprintf "P%d." i;
                                Printf.sprintf "    MOVE %d TO V1." i ])
      (List.init size Fun.id)
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    semantic tokens: 200069
    document symbols: 20015
    code lenses: 20010
    formatting edits: 0
    hover: some
    definitions: 1
    references: 20001
    rename edits: 20001
    completion items: 19 |}]

let%expect_test "long-condition" =
  let end_with_postproc =
    check @@ gen_program @@
    "    IF V0 = 0" ::
    List.init size (fun i -> Printf.sprintf "      OR V%d = %d" (i mod 10) i) @
    [ "    MOVE 1 TO V1 END-IF." ]
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    semantic tokens: 80079
    document symbols: 15
    code lenses: 10
    formatting edits: 20001
    hover: some
    definitions: 1
    references: 2002
    rename edits: 2002
    completion items: 19 |}]

let%expect_test "long-expression" =
  let end_with_postproc =
    check @@ gen_program @@
    "    COMPUTE V0 = 0" ::
    List.init size (fun i -> Printf.sprintf "      + V%d" (i mod 10)) @
    [ "    MOVE 1 TO V1." ]
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    semantic tokens: 40078
    document symbols: 15
    code lenses: 10
    formatting edits: 20000
    hover: some
    definitions: 1
    references: 2002
    rename edits: 2002
    completion items: 19 |}]
