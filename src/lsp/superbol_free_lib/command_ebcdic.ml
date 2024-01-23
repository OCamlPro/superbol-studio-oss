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

open Ezcmd.V2
open EZCMD.TYPES

open Ebcdic

let about : block list = []

let ebcdic_compare ~translation s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let rec iter i =
    if i = len1 then
      if i = len2 then
        0
      else
        -1
    else
    if i = len2 then
      1
    else
      let c1 = s1.[i] in
      let c1 = int_of_char c1 in
      let c1 = translation.of_ascii.(c1) in

      let c2 = s2.[i] in
      let c2 = int_of_char c2 in
      let c2 = translation.of_ascii.(c2) in

      let c = compare c1 c2 in
      if c = 0 then
        iter (i+1)
      else
        c
  in
  iter 0

let sort_cmd =
  let sep = ref None in
  let key = ref 0 in
  let input = ref None in
  let output = ref None in
  let translation = ref Ebcdic.default in
  EZCMD.sub
    "ebcdic sort"
    (fun () ->
       let key = !key in
       let sep = !sep in
       let ic =
         match !input with
         | None -> stdin
         | Some file ->
           open_in file
       in

       begin
         match sep with
         | None ->
           if key > 0 then begin
             Printf.eprintf "Key %d expects an inside-line separator\n%!" key;
             exit 2
           end
         | Some _ -> ()
       end;
       let rec iter lines =
         match input_line ic with
         | exception End_of_file -> List.rev lines |> Array.of_list
         | line ->
           let items = match sep with
             | None -> [| line |]
             | Some sep ->
               let items = String.split_on_char sep line |> Array.of_list in
               if Array.length items <= key then begin
                 Printf.eprintf "Line %S contains fewer items than key %d\n%!"
                   line key;
                 exit 2
               end;
               items
           in
           iter (items :: lines)
       in
       let lines = iter [] in
       begin
         match !input with
         | None -> ()
         | Some _ -> close_in ic
       end;
       let translation = !translation in
       Array.sort (fun l1 l2 ->
           let k1 = l1.(key) in
           let k2 = l2.(key) in
           ebcdic_compare ~translation k1 k2
         ) lines ;
       let oc = match !output with
         | None -> stdout
         | Some file -> open_out file
       in
       let sep = match sep with
         | None -> None
         | Some c -> Some ( String.make 1 c )
       in
       Array.iter (fun line ->
           Printf.fprintf oc "%s\n"
             (match sep with
              | None -> line.(0)
              | Some sep -> String.concat sep @@ Array.to_list line)
         ) lines;
       begin
         match !output with
         | None -> ()
         | Some _ -> close_out oc
       end
    )
    ~args:[

      [ "colseq" ], Arg.String (fun s ->
          translation := Ebcdic.read_gnucobol_collation_file s),
      EZCMD.info ~docv:"FILE" "Collating sequence file to read from GnuCOBOL";

      [ "F" ; "sep" ], Arg.String (fun s -> sep := Some s.[0]),
      EZCMD.info ~docv:"CHAR" "Separator to use between columns" ;

      [ "k" ], Arg.Int (fun i -> key := i),
      EZCMD.info ~docv:"NUM" "Key to use (0..)" ;

      [ "o" ], Arg.String (fun s -> output := Some s),
      EZCMD.info ~docv:"FILE" "File to save output in" ;

      [ ], Arg.Anon (0, fun s -> input := Some s),
      EZCMD.info ~docv:"FILE" "File to read input lines from" ;

    ]
    ~doc: "Sort a file in EBCDIC order"
    ~man:(about @ [
        `S "DESCRIPTION";
        `Blocks [
          `P "This command sorts a line file in EBCDIC order."
        ];
      ])

let ebcdic_translate_cmd =
  let file = ref None in
  let to_ascii = ref None in
  let from_ascii = ref None in
  let translation = ref Ebcdic.default in
  EZCMD.sub
    "ebcdic translate"
    (fun () ->
       match !file with
       | None ->
         Printf.eprintf "You must specify the EBCDIC filename\n%!";
         exit 2
       | Some file ->

         let translate table ~src ~dst =
           let ic = open_in_bin src in
           let oc = open_out_bin dst in
           let rec iter () =
             match input_char ic with
             | c ->
               let c = int_of_char c in
               let c = table.( c ) in
               let c = char_of_int c in
               output_char oc c ;
               iter ()
             | exception End_of_file -> ()
           in
           iter ();
           close_out oc ;
           close_in ic ;
         in
         let translation = !translation in
         begin
           match !from_ascii with
           | None -> ()
           | Some from_ascii ->
             translate translation.of_ascii ~src:from_ascii ~dst:file
         end;

         begin
           match !to_ascii with
           | None -> ()
           | Some to_ascii ->
             translate translation.to_ascii ~src:file ~dst:to_ascii
         end;

         ()
    )
    ~args:[

      [ "colseq" ], Arg.String (fun s ->
          translation := Ebcdic.read_gnucobol_collation_file s),
      EZCMD.info ~docv:"FILE" "Collating sequence file to read from GnuCOBOL";

      [], Arg.Anon (0, fun s -> file := Some s),
      EZCMD.info ~docv:"EBCDIC-FILE"
        "File in EBCDIC format (source or target)" ;

      [ "to-ascii" ], Arg.String (fun s -> to_ascii := Some s),
      EZCMD.info ~docv:"ASCII-FILE" "File in ASCII format to generate" ;

      [ "from-ascii" ], Arg.String (fun s -> from_ascii := Some s),
      EZCMD.info ~docv:"ASCII-FILE" "File in ASCII format to translate" ;

    ]
    ~doc: "Convert to or from EBCDIC"
    ~man:(about @ [
        `S "DESCRIPTION";
        `Blocks [
          `P "This command translates to or from EBCDIC and ASCII."
        ];
      ])
