(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open EzFile.OP

module MISC = Autofonce_misc.Misc

type action =
  | Apply (* really apply to file *)
  | Fake of string  (* generate files with EXTENSION instead of former
                       name *)
  | Diff of { exclude : string list ; args : string option }


type replace_block = {
  file : string ;
  line_first : int ;
  line_last : int ;
  new_content : string ;
}

let h = Hashtbl.create 100

let replace_block ~file ~line_first ~line_last new_content =
  assert ( line_last >= line_first );
  let action = {
    file ;
    line_first ;
    line_last ;
    new_content ;
  } in
  try
    let r = Hashtbl.find h file in
    r := action :: !r
  with
  | Not_found ->
      Hashtbl.add h file (ref [action])

let reset () =
  Hashtbl.clear h

let compare_actions r1 r2 =
  r1.line_first - r2.line_first

let check_consistency actions =
  match actions with
  | [] | [ _ ] -> false
  | action :: actions ->
      let rec iter a1 actions =
        match actions with
        | [] -> false
        | a2 :: actions ->
            if a1.line_last < a2.line_first then
              iter a2 actions
            else
              true
      in
      iter action actions

let commit_to_disk ?(action=Diff { exclude=[]; args=None }) ?(backup="~") () =
  let tmp_dir = Filename.temp_file "patch_lines" "dir" in
  Sys.remove tmp_dir ;
  let tmp_dir_a = tmp_dir // "a" in
  let tmp_dir_b = tmp_dir // "b" in
  let cwd = MISC.getcwd () in
  Hashtbl.iter (fun file actions ->
      let actions = List.sort compare_actions !actions in
      let inconsistent = check_consistency actions in
      if inconsistent then
        Printf.eprintf
          "Discarding inconsistent changes demanded on file %s\n%!"
          file
      else
        let old_content = EzFile.read_text_file file in
        let lines = EzString.split old_content '\n' in
        let new_lines = ref [] in
        let rec iter num lines actions =
          match actions with
          | [] ->
              new_lines := ( List.rev !new_lines ) @ lines
          | action :: actions ->
              iter_action num lines action actions

        and iter_action num lines action actions =
          match lines with
          | [] -> assert false
          | line :: lines ->
              if action.line_first > num then begin
                new_lines := line :: !new_lines;
                iter_action (num+1) lines action actions
              end else begin
                assert ( action.line_first = num );
                new_lines := action.new_content :: !new_lines;
                iter_end_action (num+1) lines action actions
              end

        and iter_end_action num lines action actions =
          match lines with
          | [] -> assert false
          | _line :: lines ->
              if action.line_last > num then begin
                iter_end_action (num+1) lines action actions
              end else begin
                assert ( action.line_last = num );
                iter (num+1) lines actions
              end
        in
        iter 1 lines actions ;
        let lines = !new_lines in
        let content = String.concat "\n" lines in
        match action with
        | Fake ext ->
            let file = file ^ ext in
            EzFile.write_text_file file content;
            Printf.eprintf "File %S updated\n%!" file
        | Apply ->
            let new_file = file ^ ".new" in
            EzFile.write_text_file new_file content;
            Sys.rename file (file ^ backup);
            Sys.rename new_file file;
            Printf.eprintf "File %S updated\n%!" file
        | Diff { exclude ; args } ->
            if not ( Sys.file_exists tmp_dir ) then begin
              Unix.mkdir tmp_dir 0o755;
              Unix.mkdir tmp_dir_a 0o755;
              Unix.mkdir tmp_dir_b 0o755;
            end;
            Unix.chdir tmp_dir ;
            let basename = Filename.basename file in
            let file_a = "a" // basename in
            let file_b = "b" // basename in
            EzFile.write_text_file file_a old_content ;
            EzFile.write_text_file file_b content ;
            let cmd = Printf.sprintf
                "diff %s %s %s %s"
                (match args with
                 | None -> "-u"
                 | Some args -> args)
                (String.concat " "
                   (List.map (fun s -> Printf.sprintf "-I '%s'" s) exclude))
                file_a file_b
            in
            Printf.printf "%s\n%!" cmd;
            ignore ( Sys.command cmd );
            Sys.remove file_a ;
            Sys.remove file_b ;


    ) h;
  Hashtbl.clear h;
  match action with
  | Diff _ ->
      if Sys.file_exists tmp_dir then begin
        Unix.rmdir tmp_dir_a ;
        Unix.rmdir tmp_dir_b ;
        Unix.rmdir tmp_dir ;
      end ;
      Unix.chdir cwd
  | _ -> ()
