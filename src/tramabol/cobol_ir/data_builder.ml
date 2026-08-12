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
open Types

open Syntax

(* --- *)

let add_field ~storage ~vm ~record ~(field: field_definition with_loc)
    ((fields_data, errs) as acc) =
  let[@local] error e =
    fields_data,
    match errs with None -> Some (NEL.one e) | Some errs -> Some (e :: errs)
  and[@local] errors e =
    fields_data,
    match errs with None -> Some e | Some errs -> Some (NEL.append e errs)
  in
  if ~&field.field_leading_ranges <> [] then
    error @@ Unsupported { stuff = Field_in_occurs; loc = ~@field }
  else if ~&field.field_length_variability = Variable_length then
    error @@ Unsupported { stuff = Variable_length_field; loc = ~@field }
  else
    match ~&field.field_qualname with
    | None ->                                            (* skip unnamed field *)
        acc
    | Some qn ->
        match vm.create_mutable_field field record with
        | Ok field_value ->
            { (* fields_data with *)
              map = FIELDS_MAP.add ~&qn field_value fields_data.map;
              working_storage =
                if storage = Working_storage
                then field_value :: fields_data.working_storage
                else fields_data.working_storage;
              local_storage =
                if storage = Local_storage
                then field_value :: fields_data.local_storage
                else fields_data.local_storage;
            },
            errs
        | Error e ->
            errors e

let create_fields_map ~vm (unit_data_defs: data_definitions)
  : (_ fields_data, error NEL.t) result =
  let fields_data =
    {
      map = FIELDS_MAP.empty;
      working_storage = [];
      local_storage = [];
    }
  in
  List.fold_left begin fun acc record_definition ->
    if record_definition.record_storage = Local_storage ||
       record_definition.record_storage = Working_storage then
      try
        let record = vm.create_record_data record_definition in
        Cobol_data.Visitor.fold_item_definition' (object
          inherit [_] Cobol_data.Visitor.folder
          method! fold_field_definition' field acc =
            Cobol_common.Visitor.proceed @@
            add_field ~vm ~record ~field acc
              ~storage:record_definition.record_storage
        end) record_definition.record_item acc
      with Cobol_data.Memory.NOT_SCALAR _ ->                (* ignored for now *)
        acc
    else
      acc
  end (fields_data, None) unit_data_defs.data_records |> function
  | fields_data, None ->
      Ok fields_data
  | _, Some errors ->
      Error errors
