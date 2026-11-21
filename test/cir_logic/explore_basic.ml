(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let%expect_test "conditional" =
  Explore_testing.explore {|
       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9.
       procedure division.
           display "start"
           if I = 1 then
             display "then"
             if I = 2 then
               display "nested"
               stop run
             end-if
           else
             display "else"
           end-if
           display "end".
  |};
  [%expect {|
    "start", "then", "nested" (status 0)
    "start", "then", "end" (status 0)
    "start", "else", "end" (status 0) |}]

let%expect_test "goto-depending" =
  Explore_testing.explore {|
       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9.
       procedure division.
           go to a b depending on I
           display "fell through".
       a.
           display "a"
           stop run.
       b.
           display "b".
  |};
  [%expect {|
    "fell through", "a" (status 0)
    "a" (status 0)
    "b" (status 0) |}]

let%expect_test "perform-times" =
  Explore_testing.explore {|
       program-id. prog.
       data division.
       working-storage section.
       77 N             PIC 9.
       procedure division.
           perform 2 times
             display "fixed"
           end-perform
           perform p N times.
           stop run.
       p.
           display "p".
  |};
  [%expect {|
    "fixed", "fixed" (status 0)
    "fixed", "fixed", "p" (status 0)
    "fixed", "fixed", "p", "p" (status 0) |}]

let%expect_test "size-error" =
  Explore_testing.explore {|
       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9.
       procedure division.
           add 1 to I
             on size error display "size error"
             not on size error display "ok"
           end-add.
  |};
  [%expect {|
    "size error" (status 0)
    "ok" (status 0) |}]

let%expect_test "unbounded-loop" =
  Explore_testing.explore ~max_steps:12 {|
       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9.
       procedure division.
           perform until I = 0
             display "loop"
           end-perform
           display "done".
  |};
  [%expect {|
    "done" (status 0)
    "loop", "done" (status 0)
    Error: Step budget exhausted (12 steps executed)

    Error: Step budget exhausted (12 steps executed) |}]

let%expect_test "exit-status" =
  Explore_testing.explore {|
       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9.
       procedure division.
           if I = 1 then
             stop I
           end-if
           goback.
  |};
  [%expect {|
    (status I)
    (status 0) |}]

let%expect_test "reentry" =
  Explore_testing.reenter {|
       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9.
       procedure division.
           goback.
  |};
  [%expect {| WORKING-STORAGE initializations: 1 |}]
