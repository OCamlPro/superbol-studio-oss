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

(*
open EzCompat (* for StringMap *)
*)
open Ezcmd.V2
open EZCMD.TYPES

open Ez_file.V1

let section_name = "util"

let about : block list = [
  `S "ABOUT UTILS" ;
  `Blocks [
    `P "This is a list of small sub-commands that may be useful from \
        time to time."  ]
]

let about man =
  man @ about

type line =
  | LINE of int
  | CYCLE of { pos : int ; size : int ; count : int }

let detect_cycle file =
  let lines = EzFile.read_lines file in
  let nlines = Array.length lines in
  Printf.eprintf "File %S has %d lines\n%!" file nlines;

  let h = Hashtbl.create (2 * nlines + 1) in
  let rec iter pos found revlines =
    if pos = nlines then
      if found then
        let newfile = file ^ "-with-cycles" in
        let oc = open_out newfile in
        Printf.eprintf "Generating file %s\n%!" newfile;
        let newlines = List.rev revlines in
        List.iter (function
            | LINE pos ->
              Printf.fprintf oc "%s\n" lines.(pos)
            | CYCLE { pos = _ ; size ; count } ->
              Printf.fprintf oc "[LAST %d LINES REPEATED %d TIMES]\n"
                size count) newlines;
        close_out oc
      else
        ()
    else
      let line = lines.( pos ) in
      match Hashtbl.find h line with
      | exception Not_found ->
        Hashtbl.add h line pos ;
        iter (pos+1) found ( LINE pos :: revlines )
      | pos0 ->
        Hashtbl.remove h line ;
        let size = pos - pos0 in
        let count : int =
          iter_matches pos0 pos 1 size 0 found in
        let pos = pos + count * size in
        if count > 0 then
          iter pos true (
            CYCLE { pos = pos0 ; size ; count } :: revlines )
        else begin
          Hashtbl.add h line pos;
          iter (pos+1) found (LINE pos :: revlines)
        end


  and iter_matches pos0 pos i size count found =

    if i = size then begin
      if not found then
        Printf.eprintf "Cycle of size %d detected at line %d\n%!" size pos;
      iter_matches pos0 (pos+size) 0 size (count+1) true
    end else
    if pos+i = nlines then
      count
    else
    if lines.(pos0 + i) = lines.(pos+i) then
      iter_matches pos0 pos (i+1) size count found
    else
      count

  in
  iter 0 false []


let util_detect_cycle_cmd =
  let files = ref [] in
  EZCMD.sub
    "util detect cycle"
    (fun () ->
       List.iter detect_cycle !files
    )
    ~args:[

      [], Arg.Anons (fun list -> files := list),
      EZCMD.info ~docv:"FILES" "Files to unrec";
    ]
    ~doc: "Detect a cycle of lines in a file"
    ~man: ( about @@ [
        `S "DESCRIPTION";
        `Blocks [
          `P "This command will take a file of lines and detect cycles \
              in the lines, and simplify them."
        ];
      ] )
