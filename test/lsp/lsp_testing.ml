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

(** This module gathers various utilities that can be used to test the LSP. *)

(* Note that for now, only a single project directory may be used, and we may
   need to clean it up after each test. *)

open Cobol_common.Srcloc.TYPES

open Lsp.Types
open Ez_file.V1
open Ez_file.FileString.OP

module StrMap = EzCompat.StringMap
module LSP = Cobol_lsp.INTERNAL

(* Used to remove full-path and lines in the test files *)
let () =
  Cobol_common.Visitor.in_testsuite := true

(** {1 Server initialization} *)

let layout =
  Superbol_free_lib.Project.layout;
and cache_config =
  LSP.Project_cache.{
    cache_storage = No_storage;
    cache_verbose = false;
  }

let init_temp_project ?(toml = "") () =
  let projdir = Superbol_testutils.Tempdir.make_n_enter "superbol-project" in
  let toml_file = projdir // layout.project_config_filename in
  EzFile.write_file toml_file toml;
  projdir

let make_server ?(with_semantic_tokens = false) () =
  LSP.Server.init ~params:{ config = { project_layout = layout;
                                       cache_config;
                                       enable_client_configs = false;
                                       force_syntax_diagnostics = true };
                            root_uri = None;
                            workspace_folders = [];
                            with_semantic_tokens;
                            with_client_file_watcher = false;
                            with_client_config_watcher = false }

let add_cobol_doc server ~projdir filename text =
  let path = projdir // filename in
  let uri = Lsp.Uri.of_path path in
  EzFile.(safe_mkdir @@ dirname path);
  EzFile.write_file path text;
  let server =
    LSP.Server.did_open
      DidOpenTextDocumentParams.{
        textDocument = TextDocumentItem.{
            languageId = "cobol"; version = 0; text; uri;
          };
      } server
  in
  print_newline ();
  server, TextDocumentIdentifier.create ~uri

(* let projdir = init_temp_project () *)
(* let projdir_regexp = Str.(regexp @@ quote projdir) *)
(* let projdir_marker = "__rootdir__" *)

(* let print_postproc jsonrpc = *)
(*   EzString.split jsonrpc '\n' |> *)
(*   List.filter_map begin function *)
(*     | s when String.trim s = "" -> None             (\* ignore json RPC header: *\) *)
(*     | s when EzString.starts_with ~prefix:"Content-Length: " s -> None *)
(*     | s -> Some (Str.global_replace projdir_regexp projdir_marker s) *)
(*   end |> *)
(*   String.concat "\n" |> *)
(*   print_endline *)

let projdir_marker = "__rootdir__"

type test_project =
  {
    projdir: string;
    end_with_postproc: string -> unit;
  }

let make_lsp_project ?toml () =
  let projdir = init_temp_project ?toml () in
  let projdir_regexp = Str.(regexp @@ quote projdir) in
  let temp_dir_name = Filename.get_temp_dir_name () in
  let end_with_postproc expected_output_string =
    (* Remove temporary project directory *)
    if EzString.starts_with ~prefix:temp_dir_name projdir
    then EzFile.remove_dir ~all:true projdir
    else Printf.eprintf "Leaving %s as is (does not look like a temporary \
                         directory)" projdir;
    (* Filter and print out results *)
    EzString.split expected_output_string '\n' |>
    List.filter_map begin function
      | s when String.trim s = "" -> None             (* ignore json RPC header: *)
      | s when EzString.starts_with ~prefix:"Content-Length: " s -> None
      | s -> Some (Str.global_replace projdir_regexp projdir_marker s)
    end |>
    String.concat "\n" |>
    print_endline
  in
  (* Force project initialization (so we can flush before the next RPC) *)
  ignore @@ LSP.Project.in_existing_dir projdir ~layout;
  print_newline ();
  { projdir; end_with_postproc }, make_server ()

(** {1 Cursor positions} *)

(** Structure returned by {!extract_position_markers} below. *)
type positions =
  {
    pos_anonymous: Position.t list;
    pos_map: Position.t StrMap.t;
  }

let position_marker = "_|_\\|_|[0-9a-zA-Z-+]+|_"
let position_or_newline_regexp = Str.(regexp @@ position_marker ^ "\\|\n")

(** [extract_position_markers text] records and removes any cursor position
    marker from [text], and returns the resulting text along with a set of
    cursor positions.

    Anonymous markers are denoted {[_|_]}. They are listed in order of
    appearance in [text] ([pos_anonymous]).

    Named markers are denoted {[_|some-name|_]}, where {[some-name]} may
    comprise alphanumeric and [+] or [-] characters.  They are recorded in
    [pos_map]. *)
let extract_position_markers text =
  let splits = Str.full_split position_or_newline_regexp text in
  let acc, _, _, positions =
    List.fold_left begin fun (acc, line, char, positions) -> function
      | Str.Text t ->
          t :: acc, line, char + String.length t, positions
      | Str.Delim "\n" ->
          "\n" :: acc, succ line, 0, positions
      | Str.Delim "_|_" ->
          acc, line, char, (line, char, None) :: positions
      | Str.Delim d ->
          let position_ident = Scanf.sscanf d "_|%s@|_" Fun.id in
          acc, line, char, (line, char, Some position_ident) :: positions
    end ([], 0, 0, []) splits
  in
  String.concat "" (List.rev acc),
  List.fold_left begin fun acc (line, character, ident) ->
    let pos = Lsp.Types.Position.create ~line ~character in
    match ident with
    | None -> { acc with pos_anonymous = pos :: acc.pos_anonymous }
    | Some id -> { acc with pos_map = StrMap.add id pos acc.pos_map }
  end { pos_anonymous = []; pos_map = StrMap.empty } positions

(** {1 Helpers to reconstruct and print source locations} *)

let srcloc_of_range ~uri : Range.t -> srcloc =
  let pos_fname = Lsp.Uri.to_path uri in
  let lines = EzFile.read_lines pos_fname in
  let char_count =
    Array.init (Array.length lines) (fun i -> String.length lines.(i)) in
  ignore @@ Array.fold_left begin fun (idx, prev_count) line_length ->
    char_count.(idx) <- line_length + prev_count + 1;   (* + newline character *)
    succ idx, char_count.(idx)
  end (0, 0) char_count;
  let lexpos_of_position Position.{ line; character } =
    let pos_bol = if line = 0 then 0 else char_count.(line - 1)
    and pos_lnum = line + 1 in
    Lexing.{ pos_fname; pos_bol; pos_lnum;
             pos_cnum = pos_bol + character }
  in
  fun Range.{ start; end_ } ->
    let start_pos = lexpos_of_position start
    and end_pos = lexpos_of_position end_ in
    Cobol_common.Srcloc.raw (start_pos, end_pos)

module UriCache = Ephemeron.K1.Make (Lsp.Uri)

(* type srcloc_resuscitator = *)
(*   { *)
(*     srcloc_of_location: Location.t -> srcloc; *)
(*     print_location: Location.t -> unit; *)
(*   } *)

(* let srcloc_resuscitator () = *)
(*   let cache: (Range.t -> srcloc) UriCache.t = UriCache.create 1 in *)
(*   let for_ ~uri = *)
(*     try UriCache.find cache uri *)
(*     with Not_found -> *)
(*       let f = srcloc_of_range ~uri in *)
(*       UriCache.replace cache uri f; *)
(*       f *)
(*   in *)
(*   let srcloc_of_location Location.{ uri; range } = (for_ ~uri) range in *)
(*   let print_location loc = *)
(*     Pretty.out "%a@." Cobol_common.Srcloc.pp_srcloc (srcloc_of_location loc) *)
(*   in *)
(*   { *)
(*     srcloc_of_location; *)
(*     print_location; *)
(*   } *)

(** Helper class that encapsulates a cache so we do not always re-compute line
    lengths and absolute character positions.  (Looks much cleaner than the
    equivalent code that is commented out above) *)
class srcloc_resuscitator_cache = object (self)
  val cache: (Range.t -> srcloc) UriCache.t =
    UriCache.create 1
  method private for_ ~uri =
    try UriCache.find cache uri
    with Not_found ->
      let f = srcloc_of_range ~uri in
      UriCache.replace cache uri f;
      f
  method of_ ~location:Location.{ uri; range } : srcloc =
    (self#for_ ~uri) range
  method pp ppf location =
    Pretty.print ppf "%a@." Cobol_common.Srcloc.pp_srcloc (self#of_ ~location)
  method print location =
    Pretty.out "%a@." Cobol_common.Srcloc.pp_srcloc (self#of_ ~location)
  method print_range_for ~uri range =
    self#print (Location.create ~uri ~range)
  method print_optional_range_for ~uri range =
    Option.iter (self#print_range_for ~uri) range
end

(* --- *)

(* let%expect_test "initialize-server" = *)
(*   ignore @@ LSP.Project.in_existing_dir projdir ~layout; *)
(*   print_postproc [%expect.output]; *)
(*   [%expect {| *)
(*     {"params":{"diagnostics":[],"uri":"file://__rootdir__/superbol.toml"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"} *)
(* |}];; *)
