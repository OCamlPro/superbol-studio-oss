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

open Types

type va_type =
  | VA of GoblintCil.typ * int
  | VA_N of GoblintCil.typ * int
  | VA_PROTO of (string * int * (string * GoblintCil.typ * GoblintCil.attributes) list) list
  | VA_FORMAT

type env = {
    enums : (string, enum) Hashtbl.t;
    comps : (string, comp) Hashtbl.t
  }

let env = {
    enums = Hashtbl.create 50;
    comps = Hashtbl.create 50;
  }

module Attr = struct
  let exists aname attributes =
    List.exists (function
        | GoblintCil.Attr (an, _apl) -> an = aname
      ) attributes

  let find aname attributes =
    List.find_map (function
        | GoblintCil.Attr (an, apl) when an = aname -> Some (apl)
        | _ -> None
      ) attributes

  let find_map aname f attributes =
    List.find_map (function
        | GoblintCil.Attr (an, apl) when an = aname -> f apl
        | _ -> None
      ) attributes

  let filter aname attributes =
    List.filter_map (function
        | GoblintCil.Attr (an, apl) when an = aname -> Some (apl)
        | _ -> None
      ) attributes

  let filter_map aname f attributes =
    List.filter_map (function
        | GoblintCil.Attr (an, apl) when an = aname -> f apl
        | _ -> None
      ) attributes

  let fold_left f acc attributes =
    List.fold_left (fun acc attr ->
        match attr with
        | GoblintCil.Attr (an, apl) -> f acc an apl
      ) acc attributes

  let rec collect = function
    | GoblintCil.TVoid (a) -> a
    | TInt (_ik, a) -> a
    | TFloat (_fk, a) -> a
    | TPtr (t, a) -> collect t @ a
    | TArray (t, _e, a) -> collect t @ a
    | TFun (t, _args, _b, a) -> collect t @ a
    | TNamed (_ti, a) -> a
    | TComp (_ti, a) -> a
    | TEnum (_ti, a) -> a
    | TBuiltin_va_list (a) -> a
end

let rec synthetize_type ?(for_struct=false) sym (ty, _al) =
  let syn_type_error line = Error.mk_error sym "synthetise_type" line ty in
  let has_const al = Attr.exists "const" al in
  let int_kind_sign ikind line =
    let open GoblintCil in
    match ikind with
    | ISChar -> Int8, Signed
    | IUChar -> Int8, Unsigned
    | IShort -> Int16, Signed
    | IUShort -> Int16, Unsigned
    | IInt -> Int32, Signed
    | IUInt -> Int32, Unsigned
    | ILong -> Int64, Signed (* Note: 32-bit on 64-bit Windows *)
    | IULong -> Int64, Unsigned
    | ILongLong -> Int64, Signed
    | IULongLong -> Int64, Unsigned
    | _ -> syn_type_error line
  in
  let float_kind fkind line =
    let open GoblintCil in
    match fkind with
    | FFloat -> Float
    | FDouble -> Double
    | _ -> syn_type_error line
  in
  let open GoblintCil in
  let raw_type = Cil.unrollTypeDeep ty in
  match raw_type with
  | TVoid (al) ->
      STVoid { const = has_const al }

  | TInt (IChar, al) ->
      STChar { const = has_const al }

  | TInt (ikind, al) ->
      let kind, sign = int_kind_sign ikind __LINE__ in
      STInt { const = has_const al; kind; sign }

  | TFloat (fkind, al) ->
      let kind = float_kind fkind __LINE__ in
      STFloat { const = has_const al; kind }

  | TEnum (ei, al) ->
      begin
        match Hashtbl.find_opt env.enums ei.ename with
        | Some (enum) -> STEnum { const = has_const al; enum }
        | None -> syn_type_error __LINE__
      end

  | TComp (ci, al) ->
      begin
        match Hashtbl.find_opt env.comps ci.cname with
        | Some (comp) -> STComp { const = has_const al; unref = true; comp }
        | None -> syn_type_error __LINE__
      end

  | TPtr (TComp (ci, al), _) ->
      begin
        match Hashtbl.find_opt env.comps ci.cname with
        | Some (comp) -> STComp { const = has_const al; unref = false; comp }
        | None -> syn_type_error __LINE__
      end

  | TPtr (ty, al) ->
      let type_ = synthetize_type ~for_struct sym (ty, []) in
      STPtr { const = has_const al; type_ }

  | TArray (ty, ex, al) ->
      let type_ = synthetize_type ~for_struct sym (ty, []) in
      let size =
        match ex with
        | Some (ex) ->
            let int = GoblintCil.Cil.(getInteger (constFold true ex)) in
            Z.to_int (Option.value ~default:Z.minus_one int)
        | _ -> -1
      in
      STArray { const = has_const al; type_; size }

  | TNamed (_) | TFun (_) | TBuiltin_va_list (_) ->
      syn_type_error __LINE__

let synthetize_arg sym (an, ty, al) i =
  let arg_type = synthetize_type sym (ty, al) in
  let arg_name =
    if an = "" then { name = Printf.sprintf "arg_%d" i; default = true }
    else { name = an; default = false } in
  let arg_kind =
    match Attr.find "CONSTANT" (al @ Attr.collect ty) with
    | Some (apl) ->
        let c =
          match apl with
          | [AInt (i)] -> string_of_int i
          | [AStr (s)] -> Printf.sprintf "\"%s\"" s
          | [ACons (s, [])] -> s
          | _ -> "0"
        in
        Constant (c)
    | None ->
        Normal
  in
  { arg_name; arg_type; arg_kind }

let synthetize_ret sym (ty, al) =
  let ret_type = synthetize_type sym (ty, al) in
  { ret_type }



let prettify_name c_name =
  let len = String.length c_name in
  let left = ref 0 in
  while !left < len && c_name.[!left] = '_' do
    left := !left + 1
  done;
  let right = ref (len-1) in
  while !right > !left && c_name.[!right] = '_' do
    right := !right - 1
  done;
  let span = 1 + !right - !left in
  String.sub c_name 0 span

let to_snake_case c_name =
  let b = Buffer.create (String.length c_name) in
  let _ =
    String.fold_left (fun last c ->
        if c <> '_' then
          Buffer.add_char b
            (if last = '_' then Char.uppercase_ascii c else c);
        c
      ) '_' c_name
  in
  Buffer.contents b



let synthetize_enum _loc (ei : GoblintCil.enuminfo) =
  let open GoblintCil in
  let typedef_opt =
    Attr.find_map "TYPEDEF" (function
        | [AStr (typedef)] -> Some (typedef)
        | _ -> None
      ) ei.eattr
  in
  let enum_name, enum_c_type =
    match typedef_opt with
    | None ->
        ei.ename, Printf.sprintf "enum %s" ei.ename
    | Some (typedef) ->
        typedef, typedef
  in
  let enum_name = prettify_name enum_name in
  let enum_module_name = to_snake_case enum_name in
  let items =
    List.map (fun (iname, _iattr, iexp, _iloc) ->
      let int = GoblintCil.Cil.(getInteger (constFold true iexp)) in
      iname, Z.to_int (Option.value ~default:Z.minus_one int)
      ) ei.eitems
  in
  let enum = {
      enum_module_name;
      enum_name;
      enum_ml_type = enum_module_name ^ ".t";
      enum_c_type;
      enum_items = items;
    }
  in
  Hashtbl.replace env.enums ei.ename enum;
  enum



let synthetize_comp loc ci =
  let open GoblintCil in
  let typedef_opt =
    Attr.find_map "TYPEDEF" (function
        | [AStr (typedef)] -> Some (typedef)
        | _ -> None
      ) ci.cattr
  in
  let comp_name, comp_c_type =
    match typedef_opt with
    | None ->
        let comp = if ci.cstruct then "struct" else "union" in
        ci.cname, Printf.sprintf "%s %s" comp ci.cname
    | Some (typedef) ->
        typedef, typedef
  in
  let comp_name = prettify_name comp_name in
  let comp_module_name = to_snake_case comp_name in
  let comp = {
      comp_module_name;
      comp_name;
      comp_ml_type = comp_module_name ^ ".t";
      comp_c_type;
      comp_fields = [];
      comp_no_constr = Attr.exists "NO_CONSTR" ci.cattr;
      comp_no_destr = Attr.exists "NO_DESTR" ci.cattr;
      comp_struct = ci.cstruct
    }
  in
  Hashtbl.replace env.comps ci.cname comp;
  let sym = Error.{
      sym_kind = if ci.cstruct then "struct" else "union";
      sym_name = ci.cname; sym_implem = true; sym_pos = loc;
    }
  in
  let fields =
    List.map (fun fi ->
        fi.fname, synthetize_type ~for_struct:true sym (fi.ftype, fi.fattr)
      ) ci.cfields
  in
  comp.comp_fields <- fields;
  comp



let get_va_type fun_name is_va attrs =
  let va, va_n, va_proto, va_format =
    Attr.fold_left (fun (va, va_n, va_proto, va_format) an apl ->
        match an, apl with
        | ("VA", [ASizeOf (ty); AInt (l)]) ->
            (ty, l) :: va, va_n, va_proto, va_format
        | ("VA_N", [ASizeOf (ty); AInt (l)]) ->
            va, (ty, l) :: va_n, va_proto, va_format
        | ("VA_PROTO",
           [(ACons (s, _) | AStr (s)); AInt (n);
            ASizeOf (TPtr (TFun (_, Some (args), _, []), []))]) ->
            va, va_n, (s, n, args) :: va_proto, va_format
        | ("VA_FORMAT", _) ->
            va, va_n, va_proto, 1 + va_format
        | _ ->
            va, va_n, va_proto, va_format
      ) ([], [], [], 0) attrs
  in

  let has_va = if va = [] then 0 else 1 in
  let has_va_n = if va_n = [] then 0 else 1 in
  let has_va_proto = if va_proto = [] then 0 else 1 in
  let has_va_format = if va_format = 0 then 0 else 1 in
  let nb_va_cat = has_va + has_va_n + has_va_proto + has_va_format in

  if nb_va_cat > 1 then
    failwith ("More than one VA category on function " ^ fun_name)
  else if nb_va_cat = 1 && not is_va then
    failwith ("VA attribute on non VA function " ^ fun_name)
  else if nb_va_cat = 0 && is_va then
    failwith ("Missing VA attribute on VA function " ^ fun_name);

  if has_va > 0 then
    let (ty, l) = List.hd va in
    Some (VA (ty, l))
  else if has_va_n > 0 then
    let (ty, l) = List.hd va_n in
    Some (VA_N (ty, l))
  else if has_va_proto > 0 then
    Some (VA_PROTO (List.rev va_proto))
  else if has_va_format > 0 then
    Some (VA_FORMAT)
  else
    None

let synthetize_stub loc (vi : GoblintCil.varinfo) =
  let open GoblintCil in
  let rt, args, is_va, _attr = Cil.splitFunctionType vi.vtype in
  let sym = Error.{
      sym_kind = "function"; sym_name = vi.vname; sym_pos = loc;
      sym_implem = not (Attr.exists "NOT_IMPLEMENTED" (Attr.collect rt));
    }
  in
  let args = Option.value ~default:[] args in
  let args = List.mapi (fun i arg -> synthetize_arg sym arg i) args in
  let va_type = get_va_type vi.vname is_va vi.vattr in
  (* Set last argument to VASize if needed *)
  let has_va_size =
    Option.fold ~none:false ~some: (function
        | VA (_) | VA_FORMAT -> false
        | VA_N (_) | VA_PROTO (_) -> true
      ) va_type
  in
  let args = (* TODO: add an attrib, warn if set several times, if not set take latest int *)
    if has_va_size && args <> [] then
      let args = List.rev args in
      let arg, args = List.hd args, List.tl args in
      List.rev ({ arg with arg_kind = VASize } :: args) (* TODO: should be an integer type *)
    else
      args
  in
  (* Set VA argument type, if any *)
  let va_args =
    Option.fold ~none:None ~some:(fun va_type ->
        match va_type with
        | VA (ty, l)
        | VA_N (ty, l) ->
            let varg = synthetize_arg sym ("va_arg", ty, []) 0 in
            let arg_name = { varg.arg_name with default = true } in
            let varg = { varg with arg_name; arg_kind = VAArg } in
            Some (VAUnroll { limit = l; varg })
        | VA_PROTO (_pl) ->
            None
        | VA_FORMAT ->
            Some (VAFormat)
      ) va_type
  in
  let ret = synthetize_ret sym (rt, []) in
  let stub = {
      fun_name = vi.vname; c_fun_name = vi.vname; args; va_args; ret;
    }
  in
  (* Generate a stub variant for each VA prototype *)
  let stubs =
    Option.fold ~none:[stub] ~some:(fun va_type ->
        match va_type with
        | VA_PROTO (pl) ->
            let i = List.length stub.args + 1 in
            List.rev_map (fun (s, n, vargs) ->
                let arg_kind = Constant (string_of_int n) in
                let args =
                  List.map (fun arg ->
                      match arg.arg_kind with
                      | Normal | Constant (_) | VAArg -> arg
                      | VASize -> { arg with arg_kind }
                    ) stub.args
                in
                let vargs =
                  List.mapi (fun j va -> synthetize_arg sym va (i+j)) vargs in
                { stub with fun_name = stub.fun_name ^ s; args = args @ vargs }
              ) pl
        | _ ->
            [stub]
      ) va_type
  in
  if sym.sym_implem then stubs else []



let synthetize ci_file =
  let open GoblintCil in
  (* Attach typedef names to their enum/struct/union *)
  let () =
    let update_attr name name_attr type_attr =
      Attr ("TYPEDEF", [AStr (name)]) :: name_attr @ type_attr
    in
    Cil.iterGlobals ci_file (fun glob ->
        match glob with
        | GType (ti, _loc) ->
            begin
              (* Unroll only through names, stop at ptr/array/fun *)
              match Cil.unrollType ti.ttype with
              | TEnum (ei, al) ->
                  ei.eattr <- update_attr ti.tname al ei.eattr
              | TComp (ci, al) ->
                  ci.cattr <- update_attr ti.tname al ci.cattr
              | _ -> ()
            end
        | _ ->
            ()
      )
  in
  let enums, comps =
    Cil.foldGlobals ci_file (fun (enums, comps) glob ->
        match glob with
        | GEnumTag (ei, loc) (* complete declaration *)
        | GEnumTagDecl (ei, loc) -> (* incomplete/forward declaration *)
            let enums =
              match synthetize_enum loc ei with
              | enum_ ->
                  enum_ :: enums
              | exception Exit ->
                  enums
            in
            enums, comps
        | GCompTag (ci, loc) (* complete declaration *)
        | GCompTagDecl (ci, loc) -> (* incomplete/forward declaration *)
            let comps =
              match synthetize_comp loc ci with
              | comp_ ->
                  comp_ :: comps
              | exception Exit ->
                  comps
            in
            enums, comps
        | _ ->
            enums, comps
      ) ([], [])
  in
  let stubs =
    GoblintCil.foldGlobals ci_file (fun stubs glob ->
      match glob with
      | GVarDecl (vi, loc) when GoblintCil.isFunctionType vi.vtype ->
          begin
            match synthetize_stub loc vi with
            | sl ->
                sl @ stubs
            | exception Exit ->
                stubs
          end
      | _ ->
          stubs
    ) []
  in
  { stubs = List.rev stubs; enums = List.rev enums; comps = List.rev comps }
