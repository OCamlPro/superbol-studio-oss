(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Evaluating watch expressions (and hovers, which use the same path). *)

open Harness
open Harness.Syntax

let vars = T_variables.vars
let src = T_variables.src

let watch_at line expression =
  fun ctx ->
    let* s = start ctx src ~bps:[bp src line] in
    evaluate s expression

let expect_number ~what expected result =
  check (digits result = Some expected) "%s: expected %d, got %S" what expected result

let suite = { suite = "Watch expressions"; tests = [

  expected "watching an elementary numeric item shows its value" vars (fun ctx ->
      let* r = watch_at T_variables.updated "N" ctx in
      expect_number ~what:"N" 5 r);

  expected "watching an alphanumeric item shows its text" vars (fun ctx ->
      let* r = watch_at T_variables.initial "NAME" ctx in
      check (contains ~sub:"ALICE" r) "NAME: got %S" r);

  expected "watching an unknown name reports it cannot be evaluated" vars (fun ctx ->
      let* r = watch_at T_variables.initial "NO-SUCH-ITEM" ctx in
      check (digits r = None) "NO-SUCH-ITEM: got %S" r);

  probe "watching a subordinate item shows its value" vars (fun ctx ->
      let* r = watch_at T_variables.initial "REC-ID" ctx in
      expect_number ~what:"REC-ID" 42 r);

  probe "watching a qualified item shows its value" vars (fun ctx ->
      let* r = watch_at T_variables.initial "REC-ID OF REC" ctx in
      expect_number ~what:"REC-ID OF REC" 42 r);

  probe "watching a table element shows its value" vars (fun ctx ->
      let* r = watch_at T_variables.updated "ITEM (2)" ctx in
      expect_number ~what:"ITEM (2)" 7 r);

  probe "watching a binary (COMP) item shows its value" vars (fun ctx ->
      let* r = watch_at T_variables.initial "C" ctx in
      expect_number ~what:"C" 77 r);

  probe "watching an arithmetic expression computes it" vars (fun ctx ->
      let* r = watch_at T_variables.updated "N + 1" ctx in
      expect_number ~what:"N + 1 (N = 5)" 6 r);

  probe "watching a numeric item shows a number, not its raw storage" vars (fun ctx ->
      let* r = watch_at T_variables.updated "N" ctx in
      check (not (String.contains r '"') && digits r = Some 5) "N: got %S" r);
] }
