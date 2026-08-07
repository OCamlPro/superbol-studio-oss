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

open Ez_file.V1

open Types

let is_heap_allocated st =
  match st with
  | STVoid (_) -> false
  | STChar (_) -> false
  | STInt { kind = (Int8 | Int16 | Int32); _ } -> false
  | STInt { kind = Int64; _ } -> true
  | STFloat (_) -> true
  | STEnum (_) -> false
  | STComp (_) -> true
  | STFun (_) -> true
  | STPtr (_) -> true
  | STArray (_) -> true

let rec ml_type ty =
  let itype k s =
    match k, s with
    | Int8, Signed -> "S8.t"
    | Int8, Unsigned -> "U8.t"
    | Int16, Signed -> "S16.t"
    | Int16, Unsigned -> "U16.t"
    | Int32, Signed -> "S32.t"
    | Int32, Unsigned -> "U32.t"
    | Int64, Signed -> "S64.t"
    | Int64, Unsigned -> "U64.t"
  in
  match ty with
  | STVoid (_) -> "unit"
  | STChar (_) -> "char"
  | STInt { kind; sign; _ } -> itype kind sign
  | STFloat { kind = Float; _ } -> "CFloat.t"
  | STFloat { kind = Double; _ } -> "CDouble.t"
  | STEnum { enum; _ } -> enum.enum_ml_type
  | STComp { comp; _ } -> Printf.sprintf "%s cptr" comp.comp_ml_type
  | STFun { fun_; _ } -> Printf.sprintf "(%s) callback" fun_.fun_ml_type
  | STPtr { type_; _ } -> Printf.sprintf "%s cptr" (ml_type type_)
  | STArray { type_; _ } -> Printf.sprintf "%s carray" (ml_type type_)

let rec c_type ty =
  let p const type_ ptr =
    let const = if const then " const" else "" in
    let ptr = if ptr then " *" else "" in
    Printf.sprintf "%s%s%s" type_ ptr const
  in
  let p_int const sign type_ ptr =
    let sign =
      match sign with
        | Signed -> "signed "
        | Unsigned -> "unsigned "
    in
    Printf.sprintf "%s%s" sign (p const type_ ptr)
  in
  let ikind = function
    | Int8 -> "char"
    | Int16 -> "short"
    | Int32 -> "int"
    | Int64 -> "long long"
  in
  let fkind = function
    | Float -> "float"
    | Double -> "double"
  in
  match ty with
  | STVoid { const } -> p const "void" false
  | STChar { const } -> p const "char" false
  | STInt { const; kind; sign } -> p_int const sign (ikind kind) false
  | STFloat { const; kind } -> p const (fkind kind) false
  | STEnum { const; enum } -> p const enum.enum_c_type false
  | STComp { const; unref; comp } -> p const comp.comp_c_type false ^ (if not unref then " *" else "")
  | STFun { const; unref; fun_ } -> p const fun_.fun_c_type false ^ (if not unref then " *" else "")
  | STPtr { const; type_ } -> p const (c_type type_) true
  | STArray { const; size = _; type_ } -> p const (c_type type_) true


let conv_to_c ty x =
  match ty with
  | STVoid (_) ->
      Printf.sprintf "!void!"
  | STChar (_) ->
      Printf.sprintf "(%s)Long_val(%s)" (c_type ty) x
  | STInt { kind = (Int8 | Int16 | Int32); _ } ->
      Printf.sprintf "(%s)Long_val(%s)" (c_type ty) x
  | STInt { kind = Int64; _ } ->
      Printf.sprintf "(%s)Int64_val(%s)" (c_type ty) x
  | STFloat (_) ->
      Printf.sprintf "(%s)Double_val(%s)" (c_type ty) x
  | STEnum (_) ->
      Printf.sprintf "(%s)Long_val(%s)" (c_type ty) x
  | STComp { unref = false; _ } ->
      Printf.sprintf "(%s)Ptr_val(%s)" (c_type ty) x
  | STComp { unref = true; _ } ->
      Printf.sprintf "*(%s *)Ptr_val(%s)" (c_type ty) x
  | STPtr (_) ->
      Printf.sprintf "(%s)Ptr_val(%s)" (c_type ty) x
  | STArray (_) ->
      Printf.sprintf "(%s)Array_val(%s)" (c_type ty) x
  | STFun (_) ->
      Printf.sprintf "(%s)Callback_val(%s)" (c_type ty) x

let conv_to_ml ty x v =
  let rec type_kind ty =
    match ty with
    | STVoid (_) -> "VOID", "NULL", 0
    | STChar (_) -> "CHAR", "NULL", 0
    | STInt { kind = Int8; sign = Signed; _ } -> "SINT8", "NULL", 0
    | STInt { kind = Int8; sign = Unsigned; _ } -> "UINT8", "NULL", 0
    | STInt { kind = Int16; sign = Signed; _ } -> "SINT16", "NULL", 0
    | STInt { kind = Int16; sign = Unsigned; _ } -> "UINT16", "NULL", 0
    | STInt { kind = Int32; sign = Signed; _ } -> "SINT32", "NULL", 0
    | STInt { kind = Int32; sign = Unsigned; _ } -> "UINT32", "NULL", 0
    | STInt { kind = Int64; sign = Signed; _ } -> "SINT64", "NULL", 0
    | STInt { kind = Int64; sign = Unsigned; _ } -> "UINT64", "NULL", 0
    | STFloat { kind = Float; _ } -> "CFLOAT", "NULL", 0
    | STFloat { kind = Double; _ } -> "CDOUBLE", "NULL", 0
    | STEnum (_) -> "CENUM", "NULL", 0
    | STComp { comp; _ } ->
        "CPTR", Printf.sprintf "mk_kind(CCOMP, NULL, sizeof(%s))" comp.comp_c_type, 0
    | STFun (_) -> "CFUN", "NULL", 0
    | STPtr { type_; _ } ->
        let id, contents, isize = type_kind type_ in
        "CPTR", Printf.sprintf "mk_kind(%s, %s, %d)" id contents isize, 0
    | STArray { type_; size; _ } ->
        let id, contents, isize = type_kind type_ in
        "CARRAY", Printf.sprintf "mk_kind(%s, %s, %d)" id contents isize, size
  in
  match ty with
  | STVoid (_) ->
      Printf.sprintf "((%s) = Val_unit)" x
  | STChar (_) ->
      Printf.sprintf "((%s) = Val_long(%s))" x v
  | STInt { kind = (Int8 | Int16 | Int32); _ } ->
      Printf.sprintf "((%s) = Val_long(%s))" x v
  | STInt { kind = Int64; _ } ->
      Printf.sprintf "((%s) = caml_copy_int64(%s))" x v
  | STFloat (_) ->
      Printf.sprintf "((%s) = caml_copy_double(%s))" x v
  | STEnum (_) ->
      Printf.sprintf "((%s) = Val_long(%s))" x v
  | STComp { comp; unref; _ } ->
      let v' = if unref then Printf.sprintf "copy(%s)" v else v in
      Printf.sprintf "Val_ptr(%s, CCOMP, NULL, sizeof(%s), %s)" x comp.comp_c_type v'
  | STFun (_) ->
      Printf.sprintf "Val_callback(%s, %s)" x v
  | STPtr { type_; _ } ->
      let id, contents, isize = type_kind type_ in
      Printf.sprintf "Val_ptr(%s, %s, %s, %d, %s)" x id contents isize v
  | STArray { type_; size; _ } ->
      let id, contents, isize = type_kind type_ in
      Printf.sprintf "Val_array(%s, %s, %s, %d, %s, %d)" x id contents isize v size



let clean_c_name = function
  | "value" -> "value_"
  | n -> n

let clean_ml_name = function
  | "val" -> "val_"
  | "type" -> "type_"
  | n -> n

let arg_ml_name_type arg =
  let t = ml_type arg.arg_type in
  let t =
    match arg.arg_kind with
    | Normal -> t | Constant (_) | VASize -> t
    | VAArg -> Printf.sprintf "%s array" t
  in
  if arg.arg_name.default then t
  else Printf.sprintf "%s:%s" (clean_ml_name arg.arg_name.name) t

let arg_val_type_name arg =
  Printf.sprintf "value %s_v" arg.arg_name.name

let arg_val_name arg =
  Printf.sprintf "%s_v" arg.arg_name.name

let arg_value ~va arg =
  match arg.arg_kind with
  | Normal -> clean_c_name arg.arg_name.name
  | Constant (c) -> c
  | VASize -> Option.fold ~none:"!va_size!" ~some:string_of_int va
  | VAArg -> "!va_arg!"

let gen_stub (stub : stub) =
  let ml_buf = Buffer.create 10000 in
  let c_buf = Buffer.create 10000 in

  (* Remove arguments that should not be passed to ML (constants + VA size) *)
  let ml_args_no_va =
    List.filter (fun a ->
        match a.arg_kind with
        | Normal -> true
        | Constant (_) | VASize -> false
        | VAArg -> failwith "Invariant broken, VAArg in stub.args"
      ) stub.args
  in

  (* Add VA argument, if any *)
  let ml_args_va =
    match stub.va_args with
    | Some (VAUnroll { limit = _; varg }) -> ml_args_no_va @ [varg]
    | Some (VAFormat) -> ml_args_no_va (* TODO *)
    | None -> ml_args_no_va
  in

  (* Remove arguments that should not be passed to C *)
  let c_args =
    List.filter (fun a ->
        match a.arg_kind with
        | Normal | Constant (_) | VASize -> true
        | VAArg -> failwith "Invariant broken, VAArg in stub.args"
      ) stub.args
  in

  let stub_fun_name =
    let c = stub.fun_name.[0] in
    if c = Char.uppercase_ascii c
       || stub.fun_name = "function"
       || stub.fun_name = "module"
       || stub.fun_name = "val"
    then Printf.sprintf "f_%s" stub.fun_name
    else stub.fun_name
  in

  let ml_fun_name = Printf.sprintf "ml_%s" stub.fun_name in

  let alloc =
    stub.va_args <> None ||
      is_heap_allocated stub.ret.ret_type ||
        List.exists (fun a -> is_heap_allocated a.arg_type) ml_args_va
  in

  (* Generate the ML signature of the stub *)
  Printf.bprintf ml_buf "external %s : " stub_fun_name;
  if ml_args_va = [] then
    Printf.bprintf ml_buf "unit"
  else
    Printf.bprintf ml_buf "%s"
      (String.concat " -> " (List.map arg_ml_name_type ml_args_va));
  Printf.bprintf ml_buf " -> %s = "
    (ml_type stub.ret.ret_type);
  if List.length ml_args_va > 5 then
    Printf.bprintf ml_buf "\"%s_bytecode\" " ml_fun_name;
  Printf.bprintf ml_buf "\"%s\"%s\n" ml_fun_name
    (if alloc then "" else" [@@noalloc]");

  (* Generate the C stub *)

  (* The prototype *)
  Printf.bprintf c_buf "value %s(%s)\n{\n" ml_fun_name
    (String.concat ", " (List.map arg_val_type_name ml_args_va));

  (* The CAMLparam line *)
  if alloc then
    begin
      let caml_params =
        List.filter_map (fun (arg : arg) ->
            if is_heap_allocated arg.arg_type then
              Some (arg_val_name arg)
            else None
          ) ml_args_va
      in
      Printf.bprintf c_buf "  CAMLparam%d(%s);\n"
        (List.length caml_params) (String.concat ", " caml_params)
    end;

  (* The CAMLlocal line *)
  if is_heap_allocated stub.ret.ret_type then
    Printf.bprintf c_buf "  CAMLlocal1(res_v);\n"
  else
    Printf.bprintf c_buf "  value res_v;\n";

  (* Extract C values from ML values (excluding VA) *)
  List.iter (fun (arg : arg) ->
      Printf.bprintf c_buf "  %s %s = %s;\n"
        (c_type arg.arg_type) (clean_c_name arg.arg_name.name)
        (conv_to_c arg.arg_type (arg_val_name arg))
    ) ml_args_no_va;
  begin
    match stub.va_args with
    | None ->
        ()
    | Some (VAFormat) ->
        ()
    | Some (VAUnroll { limit; varg }) ->
        let val_name = arg_val_name varg in
        Printf.bprintf c_buf "  const int %s_size = min(%d, Wosize_val(%s));\n"
          varg.arg_name.name limit val_name;
        Printf.bprintf c_buf "  %s %s[%d];\n"
          (c_type varg.arg_type) varg.arg_name.name limit;
        Printf.bprintf c_buf "  for (int i = 0; i < %s_size; ++i) {\n"
          varg.arg_name.name;
        Printf.bprintf c_buf "    %s[i] = %s;\n"
          varg.arg_name.name
          (conv_to_c varg.arg_type (Printf.sprintf "Field(%s, i)" val_name));
        Printf.bprintf c_buf "  }\n"
  end;

  (* Now, call the function with the C values *)
  begin
    match stub.va_args with
    | None ->
        (match stub.ret.ret_type with
         | STVoid (_) -> Printf.bprintf c_buf "  "
         | rt -> Printf.bprintf c_buf "  %s res = " (c_type rt));
        Printf.bprintf c_buf "%s(%s);\n" stub.c_fun_name
          (String.concat ", " (List.map (arg_value ~va:None) c_args))
    | Some (VAFormat) ->
        ()
    | Some (VAUnroll { limit; varg }) ->
        (match stub.ret.ret_type with
         | STVoid (_) -> ()
         | rt -> Printf.bprintf c_buf "  %s res;" (c_type rt));
        Printf.bprintf c_buf "  switch (%s_size) {\n" varg.arg_name.name;
        for i = 0 to limit do
          Printf.bprintf c_buf "  case %d:\n" i;
          (match stub.ret.ret_type with
           | STVoid (_) -> Printf.bprintf c_buf "    "
           | _ -> Printf.bprintf c_buf "    res = ");
          Printf.bprintf c_buf "%s(%s" stub.c_fun_name
            (String.concat ", " (List.map (arg_value ~va:(Some i)) c_args));
          if i > 0 then
            begin
              if c_args <> [] then Printf.bprintf c_buf ", ";
              Printf.bprintf c_buf "%s[0]" varg.arg_name.name
            end;
          for j = 1 to i-1 do
            Printf.bprintf c_buf ", %s[%d]" varg.arg_name.name j
          done;
          Printf.bprintf c_buf ");\n";
          Printf.bprintf c_buf "    break;\n"
        done;
        Printf.bprintf c_buf "  }\n"
  end;

  (* Finally, let's translate the result back to ML *)
  Printf.bprintf c_buf "  %s;\n"
    (conv_to_ml stub.ret.ret_type "res_v" "res");

  (* The CAMLreturn line *)
  if alloc then
    Printf.bprintf c_buf "  CAMLreturn(res_v);\n"
  else
    Printf.bprintf c_buf "  return res_v;\n";
  Printf.bprintf c_buf "}\n\n";

  (Buffer.contents ml_buf, Buffer.contents c_buf)



let mod_ml_type comp ty =
  match ty with
  | STComp c when c.comp == comp -> "t cptr"
  | _ -> ml_type ty

let gen_comp (comp : comp) =
  let ml_buf = Buffer.create 10000 in
  let c_buf = Buffer.create 10000 in

  (* Generate the ML module *)

  Printf.bprintf ml_buf "module %s = struct\n" comp.comp_module_name;
  Printf.bprintf ml_buf "  type k\n";
  Printf.bprintf ml_buf "  type t = k comp\n";

  (* Struct kind *)
  Printf.bprintf ml_buf
    "  external kind : unit -> k comp_kind = \"ml_comp_%s_kind\"\n"
    comp.comp_name;
  Printf.bprintf ml_buf "  let kind = kind ()\n";

  (* Casts *)
  Printf.bprintf ml_buf "  external to_ptr : t -> t cptr = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external of_ptr : t cptr -> t = \"%%identity\"\n";

  (* Null-Constructor *)
  Printf.bprintf ml_buf
    "  external null : unit -> t cptr = \"ml_comp_%s_null\"\n"
    comp.comp_name;

  (* Constructor *)
  if not comp.comp_no_constr then
    if comp.comp_struct then
      begin
        let args =
          if comp.comp_fields <> [] then
            List.map (fun (name, ty) ->
                Printf.sprintf "%s:%s"
                  (clean_ml_name name) (mod_ml_type comp ty)
              ) comp.comp_fields
          else
            ["unit"]
        in
        Printf.bprintf ml_buf "  external create : %s -> t cptr = "
          (String.concat " -> " args);
        if List.length comp.comp_fields >= 5 then
          Printf.bprintf ml_buf "\"ml_comp_%s_create_bytecode\" "
            comp.comp_name;
        Printf.bprintf ml_buf "\"ml_comp_%s_create\"\n" comp.comp_name
      end
    else
      begin
        Printf.bprintf ml_buf "  external create : unit -> t cptr = ";
        Printf.bprintf ml_buf "\"ml_comp_%s_create\"\n" comp.comp_name;
        List.iter (fun (name, ty) ->
            Printf.bprintf ml_buf "  external create_%s : %s -> t cptr = "
              name (mod_ml_type comp ty);
            Printf.bprintf ml_buf "\"ml_comp_%s_create_%s\"\n"
              comp.comp_name name;
          ) comp.comp_fields
      end;

  (* Destructor *)
  if not comp.comp_no_destr then
    begin
      Printf.bprintf ml_buf
        "  external free : t cptr -> unit = \"ml_comp_%s_free\"\n" comp.comp_name
    end;

  List.iter (fun (name, ty) ->

      (* Getter *)
      Printf.bprintf ml_buf
        "  external get_%s : t cptr -> %s = \"ml_comp_%s_get_%s\"\n"
        name (mod_ml_type comp ty) comp.comp_name name;

      (* Setter *)
      match ty with
      | STArray (_) ->
          (* Flat arrays in struct can not be set directly:
             one should get the field and set its contents *)
          ()
      | _ ->
          Printf.bprintf ml_buf
            "  external set_%s : t cptr -> %s -> unit = \"ml_comp_%s_set_%s\"\n"
            name (mod_ml_type comp ty) comp.comp_name name

    ) comp.comp_fields;

  Printf.bprintf ml_buf "end\n\n";

  (* Generate the C stub *)

  (* Struct kind *)
  Printf.bprintf c_buf "value ml_comp_%s_kind(value unit_v)\n{\n"
    comp.comp_name;
  Printf.bprintf c_buf "  return Val_long(sizeof(%s));\n" comp.comp_c_type;
  Printf.bprintf c_buf "}\n\n";

  (* Null-Constructor *)
  Printf.bprintf c_buf "value ml_comp_%s_null(value unit_v)\n{\n"
    comp.comp_name;
  Printf.bprintf c_buf "  value res_v;\n";
  Printf.bprintf c_buf "  Val_ptr(res_v, CCOMP, NULL, sizeof(%s), NULL);\n" comp.comp_c_type;
  Printf.bprintf c_buf "  return res_v;\n";
  Printf.bprintf c_buf "}\n\n";

  (* Constructor(s) *)
  if not comp.comp_no_constr then
    if comp.comp_struct then
      begin
        let args =
          List.map (fun (name, _ty) ->
              Printf.sprintf "value %s_v" name
            ) comp.comp_fields
        in
        Printf.bprintf c_buf "value ml_comp_%s_create(%s)\n{\n"
          comp.comp_name (String.concat ", " args);
        let args =
          List.map (fun (name, _ty) ->
              Printf.sprintf "%s_v" name
            ) comp.comp_fields
        in
        Printf.bprintf c_buf "  CAMLparam%d(%s);\n"
          (List.length args) (String.concat ", " args);
        Printf.bprintf c_buf "  CAMLlocal1(res_v);\n";
        Printf.bprintf c_buf "  %s *res = (%s *)calloc(1, sizeof(%s));\n"
          comp.comp_c_type comp.comp_c_type comp.comp_c_type;
        List.iter (fun (name, ty) ->
            Printf.bprintf c_buf "  res->%s = %s;\n" name
              (conv_to_c ty (Printf.sprintf "%s_v" name));
          ) comp.comp_fields;
        Printf.bprintf c_buf "  Val_ptr(res_v, CCOMP, NULL, sizeof(%s), res);\n" comp.comp_c_type;
        Printf.bprintf c_buf "  CAMLreturn(res_v);\n";
        Printf.bprintf c_buf "}\n\n"
      end
    else
      begin
        Printf.bprintf c_buf "value ml_comp_%s_create(value unit_v)\n{\n"
          comp.comp_name;
        Printf.bprintf c_buf "  value res_v;\n";
        Printf.bprintf c_buf "  %s *res = (%s *)calloc(1, sizeof(%s));\n"
          comp.comp_c_type comp.comp_c_type comp.comp_c_type;
        Printf.bprintf c_buf "  Val_ptr(res_v, CCOMP, NULL, sizeof(%s), res);\n" comp.comp_c_type;
        Printf.bprintf c_buf "  return res_v;\n";
        Printf.bprintf c_buf "}\n\n";
        List.iter (fun (name, ty) ->
            Printf.bprintf c_buf "value ml_comp_%s_create_%s(value val_v)\n{\n"
              comp.comp_name name;
            Printf.bprintf c_buf "  value res_v;\n";
            Printf.bprintf c_buf "  %s *res = (%s *)calloc(1, sizeof(%s));\n"
              comp.comp_c_type comp.comp_c_type comp.comp_c_type;
            Printf.bprintf c_buf "  res->%s = %s;\n"
              name (conv_to_c ty "val_v");
            Printf.bprintf c_buf "  Val_ptr(res_v, CCOMP, NULL, sizeof(%s), res);\n" comp.comp_c_type;
            Printf.bprintf c_buf "  return res_v;\n";
            Printf.bprintf c_buf "}\n\n";
          ) comp.comp_fields
      end;

  (* Destructor *)
  if not comp.comp_no_destr then
    begin
      Printf.bprintf c_buf "value ml_comp_%s_free(value obj_v)\n{\n"
        comp.comp_name;
      Printf.bprintf c_buf "  free(Ptr_val(obj_v));\n";
      Printf.bprintf c_buf "  Field(obj_v, 0) = (value)NULL;\n";
      Printf.bprintf c_buf "  return Val_unit;\n";
      Printf.bprintf c_buf "}\n\n"
    end;

  (* Accessors *)
  List.iter (fun (name, ty) ->

      (* Getter *)
      Printf.bprintf c_buf "value ml_comp_%s_get_%s(value obj_v)\n{\n"
        comp.comp_name name;
      Printf.bprintf c_buf "  value res_v;\n";
      Printf.bprintf c_buf "  %s *obj = (%s *)Ptr_val(obj_v);\n"
        comp.comp_c_type comp.comp_c_type;
      Printf.bprintf c_buf "  %s res = obj->%s;\n" (c_type ty) name;
      Printf.bprintf c_buf "  %s;\n" (conv_to_ml ty "res_v" "res");
      Printf.bprintf c_buf "  return res_v;\n";
      Printf.bprintf c_buf "}\n\n";

      (* Setter *)
      match ty with
      | STArray (_) ->
          (* Flat arrays in struct can not be set directly:
             one should get the field and set its contents *)
          ()
      | _ ->
          Printf.bprintf c_buf
            "value ml_comp_%s_set_%s(value obj_v, value val_v)\n{\n"
            comp.comp_name name;
          Printf.bprintf c_buf "  %s *obj = (%s *)Ptr_val(obj_v);\n"
            comp.comp_c_type comp.comp_c_type;
          Printf.bprintf c_buf "  obj->%s = %s;\n" name (conv_to_c ty "val_v");
          Printf.bprintf c_buf "  return Val_unit;\n";
          Printf.bprintf c_buf "}\n\n"

    ) comp.comp_fields;

  (Buffer.contents ml_buf, Buffer.contents c_buf)



let gen_enum (enum : enum) =
  let ml_buf = Buffer.create 10000 in

  Printf.bprintf ml_buf "module %s = struct\n" enum.enum_module_name;

  if enum.enum_items = [] then
    Printf.bprintf ml_buf "  type k\n"
  else
    begin
      Printf.bprintf ml_buf "  type k =\n";
      List.iter (fun (iname, _ival) ->
          Printf.bprintf ml_buf "    | %s\n" iname;
        ) enum.enum_items;
      Printf.bprintf ml_buf "\n";
    end;

  Printf.bprintf ml_buf "  type t = k enum\n\n";

  Printf.bprintf ml_buf "  external of_int : int -> t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external to_int : t -> int = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external of_s16 : S16.t -> t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external to_s16 : t -> S16.t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external of_u16 : U16.t -> t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external to_u16 : t -> U16.t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external of_s32 : S32.t -> t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external to_s32 : t -> S32.t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external of_u32 : U32.t -> t = \"%%identity\"\n";
  Printf.bprintf ml_buf "  external to_s32 : t -> U32.t = \"%%identity\"\n\n";

  if enum.enum_items <> [] then
    begin
      Printf.bprintf ml_buf "  let enc = function\n";
      List.iter (fun (iname, ival) ->
          Printf.bprintf ml_buf "    | %s -> of_int (%d)\n" iname ival;
        ) enum.enum_items;
      Printf.bprintf ml_buf "\n";

      Printf.bprintf ml_buf "  let dec x = match to_int x with\n";
      List.iter (fun (iname, ival) ->
          Printf.bprintf ml_buf "    | %d -> %s\n" ival iname;
        ) enum.enum_items;
      Printf.bprintf ml_buf
        "    | _ -> raise (Invalid_argument \"%s.dec\")\n\n"
        enum.enum_module_name;
    end;

  Printf.bprintf ml_buf "end\n\n";

  Buffer.contents ml_buf



let gen_fun pool_size (fun_ : fun_) =
  let ml_buf = Buffer.create 10000 in
  let c_buf = Buffer.create 10000 in

  (* Generate the ML create/free functions *)
  Printf.bprintf ml_buf
    "  external create_%s : (%s) -> (%s) t = \"ml_cb_%s_create\"\n"
    fun_.fun_sig fun_.fun_ml_type fun_.fun_ml_type fun_.fun_sig;
  Printf.bprintf ml_buf
    "  external free_%s : (%s) t -> unit = \"ml_cb_%s_free\"\n"
    fun_.fun_sig fun_.fun_ml_type fun_.fun_sig;

  (* Generate the closure array *)
  Printf.bprintf c_buf
    "static value cb_%s_closures[] = {\n" fun_.fun_sig;
  Printf.bprintf c_buf "  ";
  for _i = 0 to pool_size - 2 do
    Printf.bprintf c_buf "Val_unit, ";
  done;
  Printf.bprintf c_buf "Val_unit\n";
  Printf.bprintf c_buf "};\n\n";

  (* Generate the dispatch function *)

  (* The prototype *)
  Printf.bprintf c_buf "/*static*/ %s cb_%s_dispatch(int slot"
    (c_type fun_.fun_ret) fun_.fun_sig;
  if fun_.fun_args <> [] then
    Printf.bprintf c_buf ", %s"
      (String.concat ", "
         (List.map (fun (an, ty) ->
              Printf.sprintf "%s %s" (c_type ty) an) fun_.fun_args));
  Printf.bprintf c_buf ") {\n";

  (* The CAMLparam line *)
  Printf.bprintf c_buf "  CAMLparam0();\n";

  (* The CAMLlocal line *)
  let use_array = List.length fun_.fun_args > 3 in
  let caml_alloc, caml_non_alloc =
    if use_array then [], []
    else
      List.fold_left (fun (a, na) (an, ty) ->
          let name = Printf.sprintf "%s_v" an in
          if is_heap_allocated ty then name :: a, na else a, name :: na
        ) ([], []) (List.rev fun_.fun_args)
  in
  let caml_alloc, caml_non_alloc =
    if is_heap_allocated fun_.fun_ret then
      "res_v" :: caml_alloc, caml_non_alloc
    else
      caml_alloc, "res_v" :: caml_non_alloc
  in
  if use_array then
    Printf.bprintf c_buf "  CAMLlocalN(args_v, %d);\n" pool_size;
  if caml_alloc <> [] then
    Printf.bprintf c_buf "  CAMLlocal%d(%s);\n"
      (List.length caml_alloc) (String.concat ", " caml_alloc);
  if caml_non_alloc <> [] then
    Printf.bprintf c_buf "  value %s;\n"
      (String.concat ", " caml_non_alloc);

  (* Extract ML values from C values *)
  if use_array then
    List.iteri (fun i (an, ty) ->
        Printf.bprintf c_buf "  %s;\n"
          (conv_to_ml ty (Printf.sprintf "args_v[%d]" i) an)
      ) fun_.fun_args
  else
    List.iter (fun (an, ty) ->
        Printf.bprintf c_buf "  %s;\n"
          (conv_to_ml ty (Printf.sprintf "%s_v" an) an)
      ) fun_.fun_args;

  (* Now, call the function with the C values *)
  let nb_args = List.length fun_.fun_args in
  if nb_args <= 3 then
    Printf.bprintf c_buf
      "  res_v = caml_callback%s_exn(cb_%s_closures[slot], %s);\n"
      (if nb_args <= 1 then "" else string_of_int nb_args)
      fun_.fun_sig
      (if nb_args <= 0 then "Val_unit"
       else String.concat ", "
              (List.map (fun (an, _ty) ->
                   Printf.sprintf "%s_v" an) fun_.fun_args))
  else
    Printf.bprintf c_buf
      "  res_v = caml_callbackN_exn(cb_%s_closures[slot], %d, args_v);\n"
      fun_.fun_sig nb_args;

  (* Handle the return value *)
  begin
    match fun_.fun_ret with
    | STVoid (_) ->
        Printf.bprintf c_buf "  CAMLreturn0;\n"
    | _ ->
        Printf.bprintf c_buf "  %s res;\n" (c_type fun_.fun_ret);
        Printf.bprintf c_buf "  if (!Is_exception_result(res_v)) {\n";
        Printf.bprintf c_buf "    res = %s;\n" (conv_to_c fun_.fun_ret "res_v");
        Printf.bprintf c_buf "  }\n";
        Printf.bprintf c_buf "  CAMLreturnT(%s, res);\n" (c_type fun_.fun_ret);
  end;
  Printf.bprintf c_buf "}\n\n";

  (* Generate the trampolines *)
  for i = 0 to pool_size - 1 do
    Printf.bprintf c_buf
      "static %s cb_%s_%d(%s) { return cb_%s_dispatch(%d"
      (c_type fun_.fun_ret) fun_.fun_sig i
      (String.concat ", "
         (List.map (fun (an, ty) ->
              Printf.sprintf "%s %s" (c_type ty) an) fun_.fun_args))
      fun_.fun_sig i;
    if fun_.fun_args <> [] then
      Printf.bprintf c_buf ", %s"
        (String.concat ", " (List.map fst fun_.fun_args));
    Printf.bprintf c_buf "); }\n"
  done;
  Printf.bprintf c_buf "\n";

  (* Generate the trampoline array *)
  Printf.bprintf c_buf
    "static %s * const cb_%s_table[] = {\n"
    fun_.fun_c_type fun_.fun_sig;

  Printf.bprintf c_buf "  ";
  for i = 0 to pool_size - 2 do
    Printf.bprintf c_buf "cb_%s_%d, " fun_.fun_sig i;
  done;
  Printf.bprintf c_buf "cb_%s_%d\n" fun_.fun_sig (pool_size - 1);
  Printf.bprintf c_buf "};\n\n";

  (* Generate the C create stubs *)
  Printf.bprintf c_buf "value ml_cb_%s_create(value closure_v)\n{\n" fun_.fun_sig;
  Printf.bprintf c_buf "  static int first_slot = 0;\n";
  Printf.bprintf c_buf "  return callback_create(closure_v, (f_generic **)cb_%s_table, cb_%s_closures, &first_slot, %d);\n" fun_.fun_sig fun_.fun_sig pool_size ;
  Printf.bprintf c_buf "};\n\n";

  (* Generate the C free stubs *)
  Printf.bprintf c_buf "value ml_cb_%s_free(value callback_v)\n{\n" fun_.fun_sig;
  Printf.bprintf c_buf "  return callback_free(callback_v, (f_generic **)cb_%s_table, cb_%s_closures, %d);\n" fun_.fun_sig fun_.fun_sig pool_size;
  Printf.bprintf c_buf "};\n\n";

  (Buffer.contents ml_buf, Buffer.contents c_buf)



let add_data buf data =
  Buffer.add_string buf data

let add_file file buf suffix ext =
  let c_file_base = Filename.chop_suffix file ext in
  let c_file_header = c_file_base ^ suffix ^ ext in
  if Sys.file_exists c_file_header then begin
    let contents = EzFile.read_file c_file_header in
    Buffer.add_string buf contents
  end

let generate (contents : contents) ml_file c_file =

  let ml_buf = Buffer.create 10000 in
  Printf.bprintf ml_buf "(* generated by genstubs.ml *)\n";

  let c_buf = Buffer.create 10000 in
  Printf.bprintf c_buf "/* generated by genstubs.ml */\n";

  add_data ml_buf Common_ml.contents;
  add_data c_buf Common_c.contents;

  add_file ml_file ml_buf "_header" ".ml";
  add_file c_file c_buf "_header" ".c";

  (* Generate convenience typedefs for function pointer types *)
  List.iter (fun fun_ ->
      Printf.bprintf c_buf "typedef %s (%s)(%s);\n\n"
        (c_type fun_.fun_ret) fun_.fun_c_type
        (String.concat ", "
           (List.map (fun (_an, ty) -> c_type ty) fun_.fun_args));
    ) contents.funs;

  (* Generate enumeartions *)
  List.iter (fun enum ->
      let ml_str = gen_enum enum in
      Buffer.add_string ml_buf ml_str
    ) contents.enums;

  (* Generate structures and unions *)
  List.iter (fun comp ->
      let ml_str, c_str = gen_comp comp in
      Buffer.add_string ml_buf ml_str;
      Buffer.add_string c_buf c_str
    ) contents.comps;

  (* Generate function pointer trampolines *)
  Printf.bprintf ml_buf "module Callback = struct\n";
  Printf.bprintf ml_buf "  type 'a t = 'a callback\n";
  Printf.bprintf ml_buf "  external cast : 'a t -> 'b t = \"%%identity\"";
  List.iter (fun fun_ ->
      let ml_str, c_str = gen_fun contents.trampoline_pool_size fun_ in
      Buffer.add_string ml_buf ml_str;
      Buffer.add_string c_buf c_str
    ) contents.funs;
  Printf.bprintf ml_buf "end\n\n";

  let nfuncs = ref 0 in
  let nfailed = ref 0 in
  List.iter (fun stub ->
      incr nfuncs;
      try
        let ml_str, c_str = gen_stub stub in
        Buffer.add_string ml_buf ml_str;
        Buffer.add_string c_buf c_str
      with Exit ->
        incr nfailed
    ) contents.stubs;

  add_file ml_file ml_buf "_trailer" ".ml";
  add_file c_file c_buf "_trailer" ".c";

  EzFile.write_file ml_file (Buffer.contents ml_buf);
  EzFile.write_file c_file (Buffer.contents c_buf);

  Printf.eprintf "%d/%d stubs created\n%!"
    (!nfuncs - !nfailed) !nfuncs;

  Error.print_errors ()
