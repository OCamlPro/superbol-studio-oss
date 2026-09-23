(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Prog_printer

let performs =
  prog "performs"
    ~procedure:{|
       A SECTION.
          PERFORM B.
          GOBACK.
       B. PERFORM C.
          GOBACK.
       C. PERFORM D.
          GOBACK.
       D SECTION.
          PERFORM A.
          GOBACK.
    |}

let ambiguous_n_anonymous =
  prog "ambiguous-n-anonymous"
    ~procedure:{|
          PERFORM A.
          PERFORM B.
          GOBACK.
       B. PERFORM A.
          GOBACK.
       A SECTION.
          PERFORM A.
          PERFORM B.
          GOBACK.
       B. PERFORM A.
          GOBACK.
      *A. GOBACK.  *> forbidden redefinition of A
    |}
