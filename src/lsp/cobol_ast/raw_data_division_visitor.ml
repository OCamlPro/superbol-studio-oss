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

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

module Make
    (Data_sections: Abstract.DATA_SECTIONS) =
struct

  module Data_division =
    Raw.Data_division (Data_sections)

  module Data_sections_visitor =
    Abstract_visitor.For_data_sections (Data_sections)
  module Data_division_visitor =
    Abstract_visitor.For_data_division (Data_division)

  class virtual ['a] folder = object
    inherit ['a] Terms_visitor.folder
    inherit ['a] Data_sections_visitor.folder
    inherit ['a] Data_division_visitor.folder
  end

  let fold_data_division (v: _#folder) =
    handle v#fold_data_division
      ~continue:begin fun { file_section; working_storage_section;
                            linkage_section; communication_section;
                            local_storage_section; report_section;
                            screen_section } x -> x
        >> fold_option v file_section
          ~fold:(fun v -> v#continue_with_file_section)
        >> fold_option v working_storage_section
          ~fold:(fun v -> v#continue_with_working_storage_section)
        >> fold_option v linkage_section
          ~fold:(fun v -> v#continue_with_linkage_section)
        >> fold_option v communication_section
          ~fold:(fun v -> v#continue_with_communication_section)
        >> fold_option v local_storage_section
          ~fold:(fun v -> v#continue_with_local_storage_section)
        >> fold_option v report_section
          ~fold:(fun v -> v#continue_with_report_section)
        >> fold_option v screen_section
          ~fold:(fun v -> v#continue_with_screen_section)
      end

end
