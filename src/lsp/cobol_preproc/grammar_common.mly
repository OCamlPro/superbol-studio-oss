%{
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

%}

%attribute option [@default None]
%attribute boption [@default false]
%attribute loption list [@default []]

%%

(* TODO: add a `recovery.benign` attribute as this can be empty *)
%public l [@recovery []] [@symbol ""] (X):
 | /* empty */    { [] }
 | x = X l = l(X) { x :: l }

(* TODO: add a `recovery.benign` attribute as this can be empty *)
%public rl [@recovery []] [@symbol ""] (X):                  (* auto-recovery *)
 | x = list(X) { x }

%public ntl [@recovery []] (X):
 | x1 = X x2 = X    { [x1; x2] }
 | x = X l = ntl(X) { x :: l }

%public %inline ll(X):
 | l = ll_rev(X) { List.rev l }

%public rll_rev [@recovery []] [@symbol ""] (X):             (* auto-recovery *)
 | x = ll_rev(X) { x }

ll_rev(X):
 | /* empty */         { [] }
 | l = ll_rev(X) x = X { x :: l }

%public %inline nell(X):
 | l = nell_rev(X) { List.rev l }

nell_rev(X):
 | x = X                 { [x] }
 | l = nell_rev(X) x = X { x :: l }

%public
let rnell(X) == l = rnell_rev(X); { List.rev l }
let rnell_rev [@recovery []] (X) :=
 | x = X;                   { [x] }
 | l = rnell_rev(X); x = X; { x :: l }

%public %inline ntll(X):
 | l = ntll_rev(X) { List.rev l }

ntll_rev(X):
 | x1 = X x2 = X         { [x2; x1] }
 | l = ntll_rev(X) x = X { x :: l }

(* TODO: add a `recovery.benign` attribute *)
%public ro [@recovery None] [@symbol ""] (X):                (* auto-recovery *)
 | x = option(X)  { x }

%public %inline o (X):
 | x = option(X) { x }

%public %inline bo(X):
 | x = boption(X) { x }

%public %inline lo(X):
 | x = loption(X) { x }

%public %inline io(X):
 | x = ioption(X) { x }

%public %inline ibo(X):
 | /* nothing */ { false }
 | X             { true }

%public %inline ilo(X):
 | /* nothing */ { [] }
 | x = X         { x }

%public %inline mr(X):
 | x = midrule(X) { x }

%public %inline er(X):
 | x = endrule(X) { x }

%public %inline or_(X,Y):
 | X | Y {}

%public %inline pf(p, X):
 | p; x = X { x }

%public %inline sf(X, s):
 | x = X; s { x }

%public %inline psf(p, X, s):
| p; x = X; s { x }

%public %inline id(X):
 | x = X { x }

%%
