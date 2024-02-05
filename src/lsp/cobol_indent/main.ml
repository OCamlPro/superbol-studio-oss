(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(* This function is both used to indent complete files (when ~range is
   not specified), and file segments (with ~range), with contents
   specified (with ~contents) or not, and either returning
   modifications to apply (with no ~inplace), or applying them
   directly (with ~inplace:true) in which case it will return an empty
   list of modifications.

   If ~inplace:true, the output is written in ~filename, with
   ~filename="-" meaning stdout. If ~filename="-", ~contents must
   contain the content of the file.

   Remark: Our editor correctly handles tabulations and fixed mode
   end-of-lines comments, should we send something more specific to
   the external editor ?

*)

(* Known limitations:

* No support for directives >> EVALUATE
* Not yet understood:
  * REPOSITORY, FUNCTION, FUNCTION-ID
* Possible indentation improvements:
  * COMPUTE xxx = A
                  + B

* Tabulations \t are not yet correctly done: we only correctly deal with TAB
  in first char, and with a tab-width of 8. All other settings will lead
  to unspecified behavior.

Issues:
* 00070 Dialect specific comments
* 00100 Continuations
* 00101 Continuations
* 00164 Conditional directives
* 00228 Bad indentation IF on right side
* 00998 $DISPLAY

*)

open Ez_file.V1

open Types

(* TODO: do not try to indent lines after range. Maybe we could save a
   state at a given position, and indent a range by starting at the
   closest line with a saved state. *)
let indent
    ~source_format
    ~config
    ~dialect (* why ? we usually don't care about the dialect to indent *)
    ~filename
    ?(verbose=false)
    ?output
    ?contents
    ?range
    ()
  =
  ignore ( config ) ;
  ignore ( dialect ) ;
  let contents = match contents with
    | None -> EzFile.read_file filename
    | Some contents -> contents
  in

  let source_format = Config.source_format source_format in
  let range = match range with
    | None -> { start_line = 1; end_line = max_int }
    | Some range -> range
  in

  let config = Config.load ~source_format ~filename in

  if Engine.verbose then
    Printf.eprintf "Config: %s\n%!" (Config.to_string config);

  let (tokens, revedits) = Scanner.tokenize ~filename ~config ~contents in

  Engine.indent_tokens ~config tokens ;

  let edits =
    let edits = ref [] in
    List.iter (fun edit ->
        if edit.offset_orig <> edit.offset_modif then begin
          edits := edit :: !edits
        end
      ) revedits ;
    !edits in

  if verbose then begin
    if edits = [] then
      Printf.eprintf "File %S: good indentation\n%!" filename
    else begin
      Printf.eprintf "File %S: %d lines to modify\n%!" filename
        ( List.length edits );
      List.iter (fun edit ->
          Printf.printf "  Line %d: move from %d to %d\n%!"
            edit.lnum edit.offset_orig edit.offset_modif
        ) edits;
    end ;
  end;

  match output with
  | Some filename ->
    if edits <> [] then
      Editor.apply_edits ~contents ~edits ~range ~filename ~config ;
    []
  | None ->
    let rec iter rev edits =
      match edits with
      | [] -> List.rev rev
      | edit :: edits ->
        if edit.lnum < range.start_line || edit.lnum > range.end_line then
          iter rev edits
        else begin
          iter (edit :: rev) edits
        end
    in
    iter [] edits
