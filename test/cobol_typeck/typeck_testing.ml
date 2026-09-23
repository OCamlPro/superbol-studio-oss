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

open Cobol_common.Srcloc.INFIX

let pp_opt pp = Fmt.(option ~none:(any "<none>") pp)
let pp_also pp = Fmt.(list ~sep:(any " ALSO ") pp)

(** Shows every condition of the program along with its expansion. Mirrors
    {!Cobol_typeck.Condition.check_procedure} so conditions are expanded exactly
    the way the typechecker expands them. *)
let show_expanded_conditions ?parser_options ?source_format ?filename contents =
  Prog_typeck.typeck ?parser_options ?source_format ?filename contents |>
  Cobol_common.Diagnostics.show_n_forget ~set_status:false ~ppf:Fmt.stdout
    ~platform:Prog_common.platform |>
  begin fun Cobol_typeck.Outputs.{ group; _ } ->
    Cobol_unit.Collections.SET.iter begin fun cu ->
      let env = ~&cu.Cobol_unit.Types.unit_data.data_items.named in
      Cobol_unit.Visitor.fold_procedure object
        inherit [unit] Cobol_unit.Visitor.folder

        method! fold_condition' c () =
          Cobol_common.Visitor.skip_children @@
          Pretty.out "@[<hv 2>%a@]@."
            (pp_opt Cobol_unit.Printer.pp_expanded_cond')
            (Cobol_typeck.Condition.expand_condition env c).result

        method! fold_evaluate' eval_stmt () =
          Cobol_common.Visitor.skip_children @@
          let subjects =
            List.map
              (fun s -> (Cobol_typeck.Condition.expand_selection_subject env s).result)
              ~&eval_stmt.eval_subjects
          in
          Pretty.out "@[<hv 2>SUBJECT %a@]@."
            (pp_also (pp_opt Cobol_unit.Printer.pp_expanded_selection_subject'))
            subjects;
          let pp_object ppf = function
            | None -> Fmt.string ppf "<invalid subject>"
            | Some obj ->
                pp_opt Cobol_unit.Printer.pp_expanded_selection_object' ppf obj
          in
          List.iter begin fun branch ->
            List.iter begin fun obj_list ->
              match
                List.map2 begin fun subj obj ->
                  Option.map
                    (fun subj ->
                      (Cobol_typeck.Condition.expand_selection_object env subj obj).result)
                    subj
                end subjects obj_list
              with
              | objects ->
                  Pretty.out "@[<hv 2>WHEN %a@]@." (pp_also pp_object) objects
              | exception Invalid_argument _ ->
                  Pretty.out "WHEN <mismatching selection length>@."
            end branch.Cobol_ptree.eval_selection
          end ~&eval_stmt.eval_branches
      end ~&cu.Cobol_unit.Types.unit_procedure ()
    end group
  end
