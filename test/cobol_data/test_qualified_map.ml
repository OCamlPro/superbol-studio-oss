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

open Cobol_ptree
open Cobol_common.Srcloc.INFIX

type t =
  | Elementary of string * int
  | Group of string * int * t list

let wss =
  [Group ("Level 1", 0, [
      Group ("Level 3", 1, [
          Elementary ("Level 2", 2)]);
      Group ("Level 4", 3, [
          Elementary ("Level 2", 4)]);
    ]);
   Group ("X", 0, [
       Group ("Y", 5, [
           Elementary ("Z", 10)
         ]);
       Elementary ("Z", 11);
     ])
  ]

let unwrap = Result.get_ok
let unwrap_err = Result.get_error
let dummy_loc = Cobol_common.Srcloc.raw Lexing.(dummy_pos, dummy_pos)

let rec qualname_of_str_list: string list -> qualname = function
  | [] -> raise (Invalid_argument "The string list should not be empty")
  | hd::[] -> Name (hd &@ dummy_loc)
  | hd::tl -> Qual (hd &@ dummy_loc, qualname_of_str_list tl)


let transform wss map =
  let rec aux str_list map = function
    | Group (name, elt, elts) ->
        let str_list = name::str_list in
        List.fold_left (aux str_list)
          (Cobol_data.Qualmap.add (qualname_of_str_list str_list) elt map)
          elts
    | Elementary (name, elt) ->
        Cobol_data.Qualmap.add (qualname_of_str_list (name::str_list)) elt map
  in
  aux [] map wss

let wss = List.fold_left
    (fun map grp ->
       transform grp map)
    Cobol_data.Qualmap.empty
    wss

let wss =
  Cobol_data.Qualmap.add
    (Qual ("Level 5" &@ dummy_loc,
           Qual ("Level 3" &@ dummy_loc,
                 Name ("Level 1" &@ dummy_loc))))
    10
    wss

let elt = Alcotest.testable Format.pp_print_int (=)
(* let error = Alcotest.testable *)
(*     (fun fmt -> function `AmbiguousQualification qualname -> Format.fprintf fmt "Duplicate(@[%a@])" pp_qualname qualname) *)
(*     (=) *)

let qual n qn =
  Qual (n &@ dummy_loc, qn)

let name n: qualname =
  Name (n &@ dummy_loc)

let access_elt_1 () =
  Alcotest.(check elt) "can access simple elt"
    (Cobol_data.Qualmap.find (name "Level 1") wss) 0

let access_elt_3 () =
  Alcotest.(check elt) "can access simple sub element"
    (Cobol_data.Qualmap.find (name "Level 3") wss) 1

let access_elt_3_2 () =
  Alcotest.(check elt) "can access qualified elt"
    (Cobol_data.Qualmap.find (qual "Level 2" (name "Level 3")) wss) 2

let access_elt_4_2 () =
  Alcotest.(check elt) "can access qualified elt"
    (Cobol_data.Qualmap.find (qual "Level 2" (name "Level 4")) wss) 4

let duplicate_2 () =
  let qualname: qualname = name "Level 2" in
  Alcotest.check_raises "Not_found on ambiguous"
    Not_found (fun () -> ignore @@ Cobol_data.Qualmap.find qualname wss)

let bad_name () =
  let qualname: qualname = qual "Y" (name "Z") in
  Alcotest.check_raises "Not_found on bad name"
    Not_found (fun () -> ignore @@ Cobol_data.Qualmap.find qualname wss)

let access_elt_x_y_z () =
  let qualname: qualname = qual "Z" (qual "Y" (name "X")) in
  Alcotest.(check elt) "can access qualified elt"
    (Cobol_data.Qualmap.find qualname wss) 10

let access_elt_x_z () =
  (* /!\ this should be considered ambiguous! /!\ *)
  let qualname: qualname = qual "Z" (name "X") in
  Alcotest.(check elt) "can access partial qualified elt"
    (Cobol_data.Qualmap.find qualname wss) 11

let bad_order () =
  let qualname: qualname = qual "Z" (qual "X" (name "Y")) in
  Alcotest.check_raises "Not_found on invalid name order"
    Not_found (fun () -> ignore @@ Cobol_data.Qualmap.find qualname wss)

(* let pp_print_str_list =
  Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)

let pp_print_set fmt =
  Cobol_data.Qualmap.(fun elt ->
    pp_qualname fmt elt;
    Format.pp_print_break fmt 2 0)

let pp_print_map f fmt =
  Cobol_data.Qualmap.iter (fun l elt ->
    Format.fprintf fmt
      "Key: %a; Value: %a;\n"
      pp_qualname l
      f elt)

let pp_map fmt m =
      Format.fprintf
        fmt
        "Bindings: @[<h>%a@]@;"
        (pp_print_map Format.pp_print_int) m *)

(* let _ =
  Format.printf "%a" pp_map wss *)

let () =
  Alcotest.(run "qualified map" [
      "access", [
        test_case "Access Level 1" `Quick access_elt_1;
        test_case "Access Level 3" `Quick access_elt_3;
        test_case "Access Level 2 IN Level 3" `Quick access_elt_3_2;
        test_case "Access Level 2 IN Level 4" `Quick access_elt_4_2;
        test_case "Error on duplicate" `Quick duplicate_2;
        test_case "Error on unknown name" `Quick bad_name;
        test_case "Access Z OF Y OF X" `Quick access_elt_x_y_z;
        test_case "Access Z OF X" `Quick access_elt_x_z;
        test_case "Error on invalid order" `Quick bad_order;
      ]])
