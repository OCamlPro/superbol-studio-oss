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

open Cobol_common
open Srcloc.INFIX

module type TAGS = sig
  val loc: Srcloc.srcloc
end

module Make (Tags: TAGS) = struct
  open Cobol_ptree

  module Term = struct
    let name n : qualname = Name (n &@ Tags.loc)
    let qual n qn: qualname = Qual (n &@ Tags.loc, qn)
    let qualident x : qualident =
      { ident_name = name x &@ Tags.loc; ident_subscripts = [] }
    let ident x : scalar = QualIdent (qualident x)
    let strlit str : scalar =
      Alphanum { str; quotation = Double_quote; hexadecimal = false; runtime_repr = Native_bytes }

  end

  module Expr = struct
    open Term
    let atom s : expr with_loc = Atom s &@ Srcloc.dummy
    let ident x = atom (ident x)
    let strlit str = atom (strlit str)
    let integer x = atom (Integer x)
  end

  module Cond = struct
    open Expr
    let expr e : cond with_loc = Expr e &@ Srcloc.dummy
    let ident x = expr (ident x)
  end

end
