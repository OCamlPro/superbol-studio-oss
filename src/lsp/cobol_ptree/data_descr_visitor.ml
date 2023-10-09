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

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Data_descr

(* --- *)

class ['a] folder = object
  inherit ['a] Terms_visitor.folder
  method fold_locale_phrase       : (locale_phrase               , 'a) fold = default
  method fold_property_kind       : (property_kind               , 'a) fold = default
end

let fold_locale_phrase (v: _ #folder) =
  handle v#fold_locale_phrase
    ~continue:begin fun { locale_name; locale_size } x -> x
      >> fold_name'_opt v locale_name
      >> fold_integer v locale_size
    end

let fold_property_kind (v: _ #folder) =
  leaf v#fold_property_kind
