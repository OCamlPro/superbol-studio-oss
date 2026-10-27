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

let show_diagnostics ?(show_data = false) ?(show_whole_definitions = false)
    ?parser_options ?source_format ?filename contents =
  Prog_typeck.typeck ?parser_options ?source_format ?filename contents |>
  Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
    ~platform:Prog_common.platform |>
  begin fun Cobol_typeck.Outputs.{ group; _ } ->
    if show_data then
      Cobol_unit.Visitor.fold_unit_group object
        inherit [unit] Cobol_unit.Visitor.folder
        method! fold_item_definition' { loc; payload = def } () =
          Cobol_common.Visitor.skip_children @@
          Pretty.out "%a@[<v>Item definition: %a@]@."
            (Cobol_common.Srcloc.pp_srcloc ~platform:Prog_common.platform) loc
            Cobol_data.Printer.pp_item_definition def
        method! fold_record_renaming' { loc; payload = ren } () =
          Cobol_common.Visitor.skip_children @@
          Pretty.out "%a@[<v>Record renaming: %a@]@."
            (Cobol_common.Srcloc.pp_srcloc ~platform:Prog_common.platform) loc
            Cobol_data.Printer.pp_record_renaming ren
        method! fold_record { record_storage; record_item; _ } () =
          Cobol_common.Visitor.do_children @@
          match record_storage with
          | Generic_file _ as s ->
              Pretty.out "%a@[<v>File storage: %a@]@."
                (Cobol_common.Srcloc.pp_srcloc ~platform:Prog_common.platform)
                record_item.loc
                Cobol_data.Printer.pp_data_storage s;
          | _ ->
              ()
        method! fold_data_definitions d () =
          if show_whole_definitions then
            Cobol_common.Visitor.skip_children @@
            Pretty.out "@[<v>Whole data defintions:@;%a@]@."
              Fmt.(list ~sep:cut @@ begin fun ppf d ->
                  Pretty.print ppf "%a@[<v>Definition: %a@]"
                    (Cobol_common.Srcloc.pp_srcloc ~platform:Prog_common.platform)
                    (Cobol_data.Item.def_loc d)
                    Cobol_data.Printer.pp_data_definition d
                end)
              d.data_items.list
          else
            Cobol_common.Visitor.do_children ()
      end group ()
  end

let show_data ?show_whole_definitions
  = show_diagnostics ~show_data:true ?show_whole_definitions

(* --- *)

let pp_with_loc = Cobol_common.Srcloc.pp_with_loc
let pp_also pp = Fmt.(list ~sep:(any " ALSO ") pp)

(** Shows every condition of the typed procedure tree, as expanded by the
    typechecker. *)
let show_expanded_conditions ?parser_options ?source_format ?filename contents =
  let Cobol_typeck.Outputs.{ group; _ } =
    Prog_typeck.typeck ?parser_options ?source_format ?filename contents |>
    Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
      ~platform:Prog_common.platform
  in
  Cobol_unit.Visitor.fold_unit_group (object
    inherit [unit] Cobol_unit.Visitor.folder

    method! fold_condition_or_error' c () =
      Cobol_common.Visitor.skip_children @@
      Pretty.out "@[<hv 2>%a@]@."
        Cobol_procedure.Printer.pp_condition_or_error' c

    method! fold_evaluate' { payload = eval_stmt; _ } () =
      Cobol_common.Visitor.skip_children @@
      let open Cobol_procedure.Printer in
      Pretty.out "@[<hv 2>SUBJECT %a@]@."
        (pp_also @@ pp_with_loc @@
         pp_or_error ~err:"invalid subject" pp_selection_subject)
        eval_stmt.eval_subjects;
      List.iter (fun { Cobol_procedure.Types.eval_selection; _ } ->
          List.iter (fun objects ->
              Pretty.out "@[<hv 2>WHEN %a@]@."
                (pp_also @@ pp_with_loc @@
                 pp_or_error ~err:"invalid selection object" pp_selection_object)
                objects
            ) eval_selection
        ) eval_stmt.eval_branches
  end) group ()
