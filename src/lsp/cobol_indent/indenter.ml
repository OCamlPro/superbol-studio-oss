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

open Indent_type

(*
  source_format: source format
  str: the string representation of the source Cobol code
  rdl: the indentation error information of the source code
  range: range of file to indent
  result: the Cobol code correctly indented (string)
*)
let indenter ~source_format (str:string) (rdl:indent_record list) range =
  let do_one_record (strl:string list) (rd:indent_record) =
    let lnum = rd.lnum in
    let offset = rd.offset_modif - rd.offset_orig in
    let str = List.nth strl (lnum - 1) in
    let newstr =
      match source_format with
      | Cobol_preproc.Src_format.SF (NoIndic, FreePaging) ->
        if offset > 0 then
          let space = String.make offset ' ' in
          space^str
        else
          String.sub str (-offset) (String.length str + offset)
      | SF (FixedIndic, FixedWidth _) ->
        (* Indenting temporarily disabled in fixed format
           https://github.com/OCamlPro/superbol-studio-oss/issues/52

           Support must be improved before enabling again, in particular to
           avoid pushing content into the margin.
           https://github.com/OCamlPro/superbol-studio-oss/issues/45
          *)
        if true then str else
        let len = String.length str in
        let str1 = String.sub str 0 7 in
        let str = String.sub str 7 (len-7) in
        let str =
          if offset > 0 then
            let space = String.make offset ' ' in
            space^str
          else
            String.sub str (-offset) (String.length str + offset)
          in
        str1^str
      (* TODO *)
      | SF (XOpenIndic, FixedWidth _) -> str
      | SF (CRTIndic, FixedWidth _) -> str
      | SF (TrmIndic, FixedWidth _) -> str
      | SF (CBLXIndic, FixedWidth _) -> str
    in
    List.mapi (fun i str -> if i = lnum - 1 then newstr else str) (strl)
  in
  let strl = String.split_on_char '\n' str in
  let strl = List.fold_left (fun acc rd -> do_one_record acc rd) strl rdl in
  let strl =
    match range with
    | None -> strl
    | Some {start_line; end_line} ->
      EzList.drop (start_line - 1) @@ EzList.take end_line strl
  in
  String.concat "\n" strl

(*indent a range of file, with the default indent_config*)
let indent_range ~dialect ~source_format ~range ~filename ~contents =
  let src_format =
    (* Note: this value doesn't actually matter, it will be overriden
       immediately by [fold_source_lines] calling [on_initial_source_format]
       below. *)
    match source_format with
    | Cobol_config.Auto -> Cobol_preproc.Src_format.from_config SFFixed
    | SF source_format -> Cobol_preproc.Src_format.from_config source_format
  in
  let state =
    Cobol_preproc.fold_source_lines ~dialect ~source_format
      ~on_initial_source_format:(fun src_format st -> { st with src_format })
      ~on_compiler_directive:(fun _ { payload = cd;  _} st ->
        match cd with
        | CDirSource { payload = src_format; _ } -> { st with src_format }
        | _ -> st
      )
      ~skip_compiler_directives_text:true
      ~f:(fun _lnum line acc -> Indent_check.check_indentation line acc)
      (String { filename; contents })
      { src_format; scope = BEGIN; context  = []; acc = []; range }
  in
  (* NB: note here we ignore diagnostics *)
  let ind_recds = state.result.acc in
  indenter
    ~source_format:state.result.src_format contents ind_recds state.result.range

(*indent a range of file, with the user-defined indent_config*)
let indent_range ~dialect ~source_format ~indent_config ~range ~filename ~contents =
  begin match indent_config with
    | Some indent_config -> Indent_config.set_config ~indent_config
    | None -> ()
  end;
  indent_range ~dialect ~source_format ~range ~filename ~contents