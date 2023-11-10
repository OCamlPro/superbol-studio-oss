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

open Testing_helpers.Make (Cobol_parser.INTERNAL.Dummy.Tags)

open Cobol_unit.Qualmap

let of_list lst =
  List.fold_left (fun map (qn, value) -> add qn value map) empty lst

let show_map map =
  Pretty.out "@[<v>Map: %a@]@." (pp_qualmap Fmt.text) map;
  Pretty.out "@[<v>Rep: %a@]@." (pp_qualmap_struct Fmt.text) map

let pp_qualname ppf qn =
  Pretty.print ppf "@[<h>%a@]" Cobol_ptree.pp_qualname qn

let show_item qn map =
  try
    Pretty.out "Find %a: %a@." pp_qualname qn Fmt.text (find qn map)
  with
  | Not_found ->
      Pretty.out "Find %a: not found@." pp_qualname qn
  | Ambiguous qn' ->
      Pretty.out "Find %a: ambiguous %a@." pp_qualname qn
        (Pretty.list pp_qualname) (Lazy.force qn')

let%expect_test "empty" =
  let map = empty in
  show_map map;
  show_item Term.(name "X") map;
  [%expect  {|
    Map: {}
    Rep: {/}
    Find X: not found |}];;

let%expect_test "simple-1" =
  let map = of_list [Term.(name "X"), "X"] in
  show_map map;
  show_item Term.(name "X") map;
  show_item Term.(name "Y") map;
  show_item Term.(qual "X" @@ name "X") map;
  [%expect  {xxx|
    Map: {
      X => X
    }
    Rep: {|
      "X": Exact { binding = X => X, refined = {/} }
    |}
    Find X: X
    Find Y: not found
    Find X IN X: not found |xxx}];;

let%expect_test "replacement-1" =
  let map = of_list Term.[name "X", "X";
                          name "X", "Y"] in
  show_map map;
  show_item Term.(name "X") map;
  show_item Term.(name "Y") map;
  show_item Term.(qual "X" @@ name "X") map;
  [%expect  {xxx|
    Map: {
      X => Y
    }
    Rep: {|
      "X": Exact { binding = X => Y, refined = {/} }
    |}
    Find X: Y
    Find Y: not found
    Find X IN X: not found |xxx}];;

let%expect_test "qualif-1" =
  let map = of_list Term.[qual "X" @@ name "X", "X/X";
                          qual "X" @@ name "Y", "Y/X"] in
  show_map map;
  show_item Term.(name "X") map;
  show_item Term.(qual "X" @@ name "X") map;
  show_item Term.(qual "Y" @@ name "X") map;
  show_item Term.(qual "X" @@ name "Y") map;
  show_item Term.(qual "X" @@ qual "Y" @@ name "Z") map;
  [%expect  {xxx|
    Map: {
      X IN Y => Y/X
      X IN X => X/X
    }
    Rep: {|
      "X":
        Exact {
          binding = None,
          refined = {|
            "X": Exact { binding = X IN X => X/X, refined = {/} }
            "Y": Exact { binding = X IN Y => Y/X, refined = {/} }
          |}
        }
    |}
    Find X: ambiguous [X IN Y, X IN X]
    Find X IN X: X/X
    Find Y IN X: not found
    Find X IN Y: Y/X
    Find X IN Y IN Z: not found |xxx}];;

let%expect_test "qualif-2" =
  let map = of_list Term.[qual "Z" @@ qual "Y" @@ name "X", "X/Y/Z"] in
  show_map map;
  show_item Term.(name "X") map;                                 (* not found *)
  show_item Term.(name "Z") map;                                 (* ok *)
  show_item Term.(qual "Z" @@ name "X") map;                     (* ok *)
  [%expect  {xxx|
    Map: {
      Z IN Y IN X => X/Y/Z
    }
    Rep: {|
      "Z": Cut { Z IN Y IN X => X/Y/Z, suffix = Y IN X }
    |}
    Find X: not found
    Find Z: X/Y/Z
    Find Z IN X: X/Y/Z |xxx}];;

let%expect_test "replacement-2" =
  let map = of_list Term.[qual "Y" @@ name "X", "X";
                          name "X",             "X";
                          name "Y",             "Y";
                          qual "Y" @@ name "X", "Y"] in
  show_map map;
  show_item Term.(name "X") map;
  show_item Term.(name "Y") map;
  show_item Term.(name "Z") map;
  show_item Term.(qual "Y" @@ name "X") map;
  show_item Term.(qual "X" @@ name "X") map;
  [%expect  {xxx|
    Map: {
      Y IN X => Y
      Y => Y
      X => X
    }
    Rep: {|
      "X": Exact { binding = X => X, refined = {/} }
      "Y":
        Exact {
          binding = Y => Y,
          refined = {|
            "X": Exact { binding = Y IN X => Y, refined = {/} }
          |}
        }
    |}
    Find X: X
    Find Y: Y
    Find Z: not found
    Find Y IN X: Y
    Find X IN X: not found |xxx}];;

let%expect_test "ambiguous-2" =
  let map = of_list Term.[qual "Z" @@ qual "Y" @@ name "X", "X/Y/Z";
                          qual "Z" @@ name "X", "X/Z"] in
  show_map map;
  show_item Term.(qual "Z" @@ qual "Y" @@ name "X") map;         (* ok *)
  show_item Term.(qual "Z" @@ name "X") map;                     (* ambiguous *)
  show_item Term.(name "Z") map;                                 (* ambiguous *)
  [%expect  {xxx|
    Map: {
      Z IN Y IN X => X/Y/Z
      Z IN X => X/Z
    }
    Rep: {|
      "Z":
        Exact {
          binding = None,
          refined = {|
            "X": Exact { binding = Z IN X => X/Z, refined = {/} }
            "Y": Cut { Z IN Y IN X => X/Y/Z, suffix = X }
          |}
        }
    |}
    Find Z IN Y IN X: X/Y/Z
    Find Z IN X: ambiguous [Z IN Y IN X, Z IN X]
    Find Z: ambiguous [Z IN Y IN X, Z IN X] |xxx}];;

let%expect_test "ambiguous-3" =
  let map = of_list Term.[qual "Z" @@ name "X", "X/Z";
                          qual "Z" @@ qual "Y" @@ name "X", "X/Y/Z"] in
  show_map map;
  show_item Term.(qual "Z" @@ qual "Y" @@ name "X") map;         (* ok *)
  show_item Term.(qual "Z" @@ name "X") map;                     (* ambiguous *)
  show_item Term.(name "Z") map;                                 (* ambiguous *)
  [%expect  {xxx|
    Map: {
      Z IN Y IN X => X/Y/Z
      Z IN X => X/Z
    }
    Rep: {|
      "Z":
        Exact {
          binding = None,
          refined = {|
            "X": Exact { binding = Z IN X => X/Z, refined = {/} }
            "Y": Cut { Z IN Y IN X => X/Y/Z, suffix = X }
          |}
        }
    |}
    Find Z IN Y IN X: X/Y/Z
    Find Z IN X: ambiguous [Z IN Y IN X, Z IN X]
    Find Z: ambiguous [Z IN Y IN X, Z IN X] |xxx}];;

let%expect_test "ambiguous-4" =
  let map = of_list Term.[qual "Z" @@ qual "Y" @@ name "X", "X/Y/Z";
                          qual "Z" @@ name "Y", "Y/Z";
                          qual "Z" @@ name "X", "X/Z";
                          qual "T" @@ qual "Y" @@ name "X", "X/Y/T" ] in
  show_map map;
  show_item Term.(name "X") map;                                 (* not found *)
  show_item Term.(name "T") map;                                 (* ok *)
  show_item Term.(qual "Z" @@ name "X") map;                     (* ambiguous *)
  show_item Term.(qual "Y" @@ name "X") map;                     (* not found *)
  show_item Term.(qual "T" @@ name "X") map;                     (* ok *)
  show_item Term.(qual "X" @@ qual "Y" @@ name "Z") map;         (* not found *)
  [%expect  {xxx|
    Map: {
      Z IN Y IN X => X/Y/Z
      Z IN Y => Y/Z
      Z IN X => X/Z
      T IN Y IN X => X/Y/T
    }
    Rep: {|
      "T": Cut { T IN Y IN X => X/Y/T, suffix = Y IN X }
      "Z":
        Exact {
          binding = None,
          refined = {|
            "X": Exact { binding = Z IN X => X/Z, refined = {/} }
            "Y":
              Exact {
                binding = Z IN Y => Y/Z,
                refined = {|
                  "X": Exact { binding = Z IN Y IN X => X/Y/Z, refined = {/} }
                |}
              }
          |}
        }
    |}
    Find X: not found
    Find T: X/Y/T
    Find Z IN X: ambiguous [Z IN Y IN X, Z IN X]
    Find Y IN X: not found
    Find T IN X: X/Y/T
    Find X IN Y IN Z: not found |xxx}];;

let%expect_test "ambiguous-5" =
  let map = of_list Term.[qual "Z" @@ qual "Y" @@ qual "X" @@ name "V", "V/X/Y/Z";
                          qual "Z" @@ qual "Y" @@ qual "X" @@ name "W", "W/X/Y/Z";
                          qual "Y" @@ qual "X" @@ name "V", "V/X/Y";
                          qual "X" @@ qual "X" @@ name "V", "V/X/X";
                          qual "X" @@ qual "Y" @@ name "W", "W/Y/X";
                          qual "Z" @@ qual "Y" @@ name "W", "W/Y/Z";
                          qual "T" @@ qual "Y" @@ name "W", "W/Y/T" ] in
  show_map map;
  show_item Term.(name "X") map;                                 (* ambiguous *)
  show_item Term.(qual "X" @@ name "V") map;                     (* V/X/X *)
  show_item Term.(qual "X" @@ name "W") map;                     (* W/Y/X *)
  show_item Term.(qual "Z" @@ name "V") map;                     (* V/X/Y/Z *)
  show_item Term.(qual "Z" @@ name "W") map;                     (* ambiguous *)
  show_item Term.(qual "Z" @@ qual "Y" @@ name "W") map;         (* ambiguous *)
  show_item Term.(qual "Z" @@ qual "X" @@ name "W") map;         (* W/X/Y/Z *)
  [%expect  {xxx|
    Map: {
      Z IN Y IN X IN W => W/X/Y/Z
      Z IN Y IN X IN V => V/X/Y/Z
      Z IN Y IN W => W/Y/Z
      Y IN X IN V => V/X/Y
      X IN Y IN W => W/Y/X
      X IN X IN V => V/X/X
      T IN Y IN W => W/Y/T
    }
    Rep: {|
      "T": Cut { T IN Y IN W => W/Y/T, suffix = Y IN W }
      "X":
        Exact {
          binding = None,
          refined = {|
            "X": Cut { X IN X IN V => V/X/X, suffix = V }
            "Y": Cut { X IN Y IN W => W/Y/X, suffix = W }
          |}
        }
      "Y": Cut { Y IN X IN V => V/X/Y, suffix = X IN V }
      "Z":
        Exact {
          binding = None,
          refined = {|
            "Y":
              Exact {
                binding = None,
                refined = {|
                  "W": Exact { binding = Z IN Y IN W => W/Y/Z, refined = {/} }
                  "X":
                    Exact {
                      binding = None,
                      refined = {|
                        "V":
                          Exact {
                            binding = Z IN Y IN X IN V => V/X/Y/Z,
                            refined = {/}
                          }
                        "W":
                          Exact {
                            binding = Z IN Y IN X IN W => W/X/Y/Z,
                            refined = {/}
                          }
                      |}
                    }
                |}
              }
          |}
        }
    |}
    Find X: ambiguous [X IN Y IN W, X IN X IN V]
    Find X IN V: V/X/X
    Find X IN W: W/Y/X
    Find Z IN V: V/X/Y/Z
    Find Z IN W: ambiguous [Z IN Y IN X IN W, Z IN Y IN W]
    Find Z IN Y IN W: ambiguous [Z IN Y IN X IN W, Z IN Y IN W]
    Find Z IN X IN W: W/X/Y/Z |xxx}];;

let%expect_test "misc-1" =
  let map = of_list
      Term.[                           name "L1",  "0"
           ;              qual "L3" @@ name "L1",  "1"
           ; qual "L2" @@ qual "L3" @@ name "L1",  "2"
           ; qual "L5" @@ qual "L3" @@ name "L1", "10"
           ;              qual "L4" @@ name "L1",  "3"
           ; qual "L2" @@ qual "L4" @@ name "L1",  "4"
           ;                           name "X",   "0"
           ;              qual "Y"  @@ name "X",   "5"
           ; qual "Z"  @@ qual "Y"  @@ name "X",  "10"
           ;              qual "Z"  @@ name "X",  "11"
           ]
  in
  show_map map;
  show_item Term.(name "L1") map;                                (* 0 *)
  show_item Term.(name "L3") map;                                (* 1 *)
  show_item Term.(qual "L2" @@ name "L3") map;                   (* 2 *)
  show_item Term.(qual "L2" @@ name "L4") map;                   (* 4 *)
  show_item Term.(name "L2") map;                                (* ambiguous *)
  show_item Term.(qual "L5" @@ name "L1") map;                   (* 10 *)
  show_item Term.(qual  "Y" @@ name  "Z") map;                   (* not found *)
  show_item Term.(qual  "Z" @@ qual  "Y" @@ name "X") map;       (* 10 *)
  show_item Term.(qual  "Z" @@ qual  "X" @@ name "Y") map;       (* not found *)
  show_item Term.(qual  "Z" @@ name  "X") map;                   (* ambiguous *)
  [%expect  {xxx|
    Map: {
      Z IN Y IN X => 10
      Z IN X => 11
      Y IN X => 5
      X => 0
      L5 IN L3 IN L1 => 10
      L4 IN L1 => 3
      L3 IN L1 => 1
      L2 IN L4 IN L1 => 4
      L2 IN L3 IN L1 => 2
      L1 => 0
    }
    Rep: {|
      "L1": Exact { binding = L1 => 0, refined = {/} }
      "L2":
        Exact {
          binding = None,
          refined = {|
            "L3": Cut { L2 IN L3 IN L1 => 2, suffix = L1 }
            "L4": Cut { L2 IN L4 IN L1 => 4, suffix = L1 }
          |}
        }
      "L3": Cut { L3 IN L1 => 1, suffix = L1 }
      "L4": Cut { L4 IN L1 => 3, suffix = L1 }
      "L5": Cut { L5 IN L3 IN L1 => 10, suffix = L3 IN L1 }
      "X": Exact { binding = X => 0, refined = {/} }
      "Y": Cut { Y IN X => 5, suffix = X }
      "Z":
        Exact {
          binding = None,
          refined = {|
            "X": Exact { binding = Z IN X => 11, refined = {/} }
            "Y": Cut { Z IN Y IN X => 10, suffix = X }
          |}
        }
    |}
    Find L1: 0
    Find L3: 1
    Find L2 IN L3: 2
    Find L2 IN L4: 4
    Find L2: ambiguous [L2 IN L4 IN L1, L2 IN L3 IN L1]
    Find L5 IN L1: 10
    Find Y IN Z: not found
    Find Z IN Y IN X: 10
    Find Z IN X IN Y: not found
    Find Z IN X: ambiguous [Z IN Y IN X, Z IN X] |xxx}];;
