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

open Cobol_common.Srcloc.TYPES
open Cobol_unit.Types
open Cobol_data.Types
open Cir_types
open Types

open Syntax

(* --- *)

type ('f, 'r) accumulator =
  {
    data: ('f, 'r) data;
    skipped_tables: table_definition with_loc list;
    errors: Types.errors option;
  }

let error acc e =
  { acc with
    errors =
      match acc.errors with None -> Some (NEL.one e) | Some errs -> Some (e :: errs) }

let errors acc e =
  { acc with
    errors =
      match acc.errors with None -> Some e | Some errs -> Some (NEL.append e errs) }

let continue_on_error ~acc = fun x f ->
  match x with
  | Ok x -> f x
  | Error e -> errors acc e

let skip acc table_definition =
  { acc with skipped_tables = table_definition :: acc.skipped_tables }

let add_record storage record =
  { storage with storage_records = record :: storage.storage_records }

let add_field storage field =
  { storage with storage_fields = field :: storage.storage_fields }

(* --- *)

let resolve_leading_ranges ~data_map leading_ranges =
  List.fold_left begin fun resolved_ranges range ->
    let* resolved_ranges in
    match range.range_span with
    | Fixed_span { occurs_times } ->
        Ok (Fixed_range { max = ~&occurs_times } :: resolved_ranges)
    | Depending_span { occurs_depending_min;
                       occurs_depending_max;
                       occurs_depending } ->
        (match FIELDS_MAP.find ~&occurs_depending data_map with
         | Direct_access odo_field ->
             Ok (Depending_range { min = ~&occurs_depending_min;
                                   max = ~&occurs_depending_max;
                                   odo_field } :: resolved_ranges)
         | Indirect_access _ ->
             Ok resolved_ranges                                     (* error! *)
         | exception Not_found ->
             (* CHECKME: Not_found -> invalid ODO? (or just skip in that
                case)? *)
             Ok resolved_ranges)                                    (* error! *)
    | _ ->
        Ok resolved_ranges
  end (Ok []) leading_ranges

(* Does nothing on unnamed fields *)
let add_field ~builder ~storage ~record
    ~(field_definition: field_definition with_loc) acc =
  let ( let* ) x f = continue_on_error ~acc x f in
  match ~&field_definition.field_qualname with
  | None ->                                              (* skip unnamed field *)
      acc
  | Some qn ->
      if ~&field_definition.field_length_variability = Variable_length then
        error acc @@ Unsupported { stuff = Variable_length_field;
                                   loc = ~@field_definition }
      else
        let* rev_ranges =
          resolve_leading_ranges ~&field_definition.field_leading_ranges
            ~data_map:acc.data.map
        and* field_value =
          builder.create_field_from_definition field_definition record
        and* field_initial_value =
          match ~&field_definition.field_layout with
          | Elementary_field { init_value = Some v; _ } ->
              Result.map Option.some @@
              builder.create_field_from_literal_value v
          | Elementary_field _
          | Struct_field _ ->
              Ok None
        in
        let fixed_field_info = { field_initial_value; field_definition } in
        let fixed_field = { fixed_field = field_value; fixed_field_info } in
        let field_access =
          match rev_ranges with
          | [] ->
              Direct_access fixed_field
          | rev_ranges ->
              Indirect_access { ranges = NEL.of_rev_list rev_ranges;
                                base_field = fixed_field }
        in
        { acc with
          data =
            { map = FIELDS_MAP.add ~&qn field_access acc.data.map;
              working_storage =
                if storage = Working_storage
                then add_field acc.data.working_storage field_access
                else acc.data.working_storage;
              local_storage =
                if storage = Local_storage
                then add_field acc.data.local_storage field_access
                else acc.data.local_storage } }

let definitions_visitor ~builder ~record ~storage ~skip_depending_tables =
  object
    inherit [_] Cobol_data.Visitor.folder
    method! fold_field_definition' field_definition acc =
      Cobol_common.Visitor.proceed @@
      add_field ~builder ~record ~field_definition ~storage acc
    method! fold_table_definition' table_definition acc =
      (* CHECKME: other special handling needed?  Maybe to detect overlapping
         initialization/VALUE clauses? *)
      match ~&table_definition.table_range.range_span with
      | Depending_span _
      | Dynamic_span _
        when skip_depending_tables ->
          Cobol_common.Visitor.skip @@ skip acc table_definition
      | Depending_span _
      | Fixed_span _
      | Dynamic_span _ ->
          Cobol_common.Visitor.proceed acc

  end

let add_record_items ~builder ~record ~storage record_items acc =
  Cobol_data.Visitor.fold_record
    (definitions_visitor ~builder ~record ~storage
       ~skip_depending_tables:true)
    record_items acc

let add_table_definitions ~builder ~record ~storage table_definitions acc =
  List.fold_left begin fun acc table_definition ->
    Cobol_data.Visitor.fold_table_definition'
      (definitions_visitor ~builder ~record ~storage
         ~skip_depending_tables:false)
      table_definition acc
  end acc table_definitions

let add_record ~builder record_definition acc =
  let[@local] ( let* ) x f = continue_on_error ~acc x f in
  if record_definition.record_storage = Local_storage ||
     record_definition.record_storage = Working_storage then
    let* record = builder.create_record_data record_definition in
    let data =
      if record_definition.record_storage = Local_storage
      then { acc.data with
             local_storage = add_record acc.data.local_storage record }
      else { acc.data with
             working_storage = add_record acc.data.working_storage record }
    in
    let acc =
      add_record_items ~builder
        record_definition { acc with data }
        ~record ~storage:record_definition.record_storage
    in
    add_table_definitions ~builder
      acc.skipped_tables { acc with skipped_tables = [] }
      ~record ~storage:record_definition.record_storage
  else
    acc

let create ~builder (unit_data_defs: data_definitions)
  : (_ data, error NEL.t) result =
  let empty_data =
    let empty_storage = { storage_records = []; storage_fields = [] } in
    { map = FIELDS_MAP.empty;
      working_storage = empty_storage;
      local_storage = empty_storage }
  in
  match
    let acc = { data = empty_data; skipped_tables = []; errors = None } in
    List.fold_left begin fun acc record_definition ->
      add_record ~builder record_definition acc
    end acc unit_data_defs.data_records
  with
  | { errors = None; data; _ } ->
      Ok data
  | { errors = Some errors; _ } ->                       (* TODO: dealloc data *)
      Error errors
