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

module IDL_PARSER : sig

  module TYPES : sig
    type c_type = string

    type file = {
      file_name : string ;
      file_contents : string ;
    }

    type c_decl = {
      fun_name : string ;
      fun_cname : string ;
      fun_return : c_type ;
      fun_args : (c_type * string option) list ;
      fun_attrs : string list ;
      fun_flags : string list ;
      fun_file : file ;
      fun_pos : int ;
    }

  end
  open TYPES

  val parse_file : string -> c_decl list
  val error_msg : string -> file -> int -> unit

end = struct

  module TYPES = struct
    type c_type = string

    type file = {
      file_name : string ;
      file_contents : string ;
    }

    type c_decl = {
      fun_name : string ;
      fun_cname : string ;
      fun_return : c_type ;
      fun_args : (c_type * string option) list ;
      fun_attrs : string list ;
      fun_flags : string list ;
      fun_file : file ;
      fun_pos : int ;
    }
  end
  open TYPES

  open Genlex
  let lexer = Genlex.make_lexer [ "("; ")"; ","; ";"; "//"; "/*"; "*/"; "." ]

  (*
  let string_of_token = function
    | Ident s -> Printf.sprintf " Ident %S ::" s
    | Kwd s -> Printf.sprintf " Kwd %S ::" s
    | String s -> Printf.sprintf "String %S ::" s
    | Int n -> Printf.sprintf "Int %d ::" n
    | Float f -> Printf.sprintf "Float %f ::" f
    | Char c -> Printf.sprintf "Char '%c' ::" c
  *)

  let is_alpha c =
    match c with
    | 'a'..'z' | 'A'..'Z' -> true
    | _ -> false

  let rec is_full_type fun_arg_rev =
    match fun_arg_rev with
    | [] -> false
    | "enum" :: fun_arg_rev -> is_full_type fun_arg_rev
    | "const" :: fun_arg_rev -> is_full_type fun_arg_rev
    | "unsigned" :: fun_arg_rev -> is_full_type fun_arg_rev
    | _ -> true

  let pair_of_arg_rev fun_arg_rev =
    match fun_arg_rev with
    | name :: fun_arg_rev
      when is_alpha name.[0]
        && is_full_type fun_arg_rev
        && (match name with
              "const" -> false
            | _ -> true)
      ->
        String.concat " " @@ List.rev fun_arg_rev, Some name
    | _ ->
        String.concat " " @@ List.rev fun_arg_rev, None

  let error_msg msg file pos =
    let nlines = ref 1 in
    let last_pos = ref 0 in
    for i = 0 to pos-1 do
      let c = file.file_contents.[i] in
      if c = '\n' then begin
        incr nlines;
        last_pos := i;
      end;
    done;
    let nchars = ref 1 in
    for i = !last_pos+1 to pos-1 do
      incr nchars;
      if file.file_contents.[i] = '\t' then
        nchars := !nchars + (6 - (!nchars mod 7))
    done;
    Printf.eprintf "src/ezlibcob/%s:%d:%d: error: %s\n%!"
      file.file_name !nlines !nchars msg

  let parse_file file_name =
    let file_contents = EzFile.read_file file_name in
    let file = {
      file_name ;
      file_contents ;
    } in
    let stream = Stream.of_string file_contents in
    let eof = String.length file_contents in
    let token_stream = lexer stream in
    let tokens =
        let rec iter rev =
          let pos = Stream.count stream in
          match Stream.next token_stream with
          | token ->
              iter ( (token, pos) :: rev )
          | exception Stream.Failure ->
              List.rev rev
          | exception exn ->
              error_msg (Printexc.to_string exn) file pos;
              exit 2
        in
        iter []
    in

    let rec iter c_decls_rev fun_return_rev fun_attrs tokens =
      match tokens with
      | (Kwd "/*", pos) :: tokens
      | (Kwd "*/", pos) :: tokens
      | (Kwd "//", pos) :: tokens
      | (Ident "extern", pos) :: tokens
      | (Ident "COB_EXPIMP", pos) :: tokens
        ->
          if fun_return_rev <> [] then
            error_msg "unexpected COB_EXPIMP" file pos;
          iter c_decls_rev [] fun_attrs tokens
      | (Ident "NOT_IMPLEMENTED", _pos) :: tokens ->
          iter c_decls_rev [] ("NOT_IMPLEMENTED" :: fun_attrs) tokens
      | (Ident "DECLNORET", _pos) :: tokens ->
          iter c_decls_rev [] ("DECLNORET" :: fun_attrs) tokens
      | (Ident fun_name, fun_pos) :: (Kwd "(", _) ::
        (Ident "void", _) :: ( ( (Kwd ")",_) :: _ ) as tokens ) ->
          iter_args
            (fun_pos, c_decls_rev, fun_name, fun_name,
             List.rev fun_return_rev, fun_attrs)
            [] [] tokens
      | (Ident fun_name, fun_pos) :: (Kwd "(", _) :: tokens ->
          iter_args
            (fun_pos, c_decls_rev, fun_name, fun_name,
             List.rev fun_return_rev, fun_attrs)
            [] [] tokens
      | (Ident fun_cname, fun_pos) :: (String fun_name, _) ::
        (Kwd "(", _) :: tokens
      | (String fun_name, _) :: (Ident fun_cname, fun_pos) ::
        (Kwd "(", _) :: tokens
        ->
          iter_args
            (fun_pos, c_decls_rev, fun_name, fun_cname,
             List.rev fun_return_rev, fun_attrs)
            [] [] tokens
      | (Ident type_part, _) :: tokens ->
          iter c_decls_rev (type_part :: fun_return_rev) fun_attrs tokens
      | [] ->
          assert (fun_return_rev = []);
          List.rev c_decls_rev
      | (_, pos) :: _ ->
          error_msg "function prototype expected" file pos;
          exit 2

    and iter_args acc fun_args_rev fun_arg_rev tokens =
      match tokens with
      | (Kwd ")", _) :: (Kwd ";", _) :: tokens ->
          iter_fun acc fun_args_rev fun_arg_rev [] tokens
      | (Kwd ")", _) :: (Ident fun_flag, _) :: (Kwd ";", _) :: tokens ->
          iter_fun acc fun_args_rev fun_arg_rev [ fun_flag ] tokens
      | (Kwd ",", _) :: tokens ->
          assert (fun_arg_rev <> []);
          let fun_args_rev = (pair_of_arg_rev fun_arg_rev) :: fun_args_rev in
          iter_args acc fun_args_rev [] tokens
      | (Kwd ".", _) :: (Kwd ".", _) :: (Kwd ".", _) :: tokens ->
          let fun_arg_rev = "..." :: fun_arg_rev in
          iter_args acc fun_args_rev fun_arg_rev tokens
      | (Ident arg_part, _) :: tokens ->
          let fun_arg_rev = arg_part :: fun_arg_rev in
          iter_args acc fun_args_rev fun_arg_rev tokens
      | (_, pos) :: _ ->
          error_msg "function argument expected" file pos;
          exit 2
      | [] ->
          error_msg "unexpected end of file before arg" file eof;
          exit 2

  and iter_fun acc fun_args_rev fun_arg_rev fun_flags tokens =
    let (fun_pos, c_decls_rev, fun_name, fun_cname, fun_return, fun_attrs) =
      acc in
          let fun_args_rev =
            match fun_arg_rev with
            | [] -> fun_args_rev
            | _ -> pair_of_arg_rev fun_arg_rev :: fun_args_rev
          in
          let fun_args = List.rev fun_args_rev in
          let c_decls_rev = {
            fun_attrs ;
            fun_name ;
            fun_cname ;
            fun_return = String.concat " " fun_return;
            fun_args ;
            fun_flags ;
            fun_file = file ;
            fun_pos ;
          } :: c_decls_rev
          in
          iter c_decls_rev [] [] tokens

    in
    iter [] [] [] tokens

end
open IDL_PARSER.TYPES

let errors = Hashtbl.create 111

let error func fun_name line ty =
  let key = (fun_name, ty) in
  let r =
    match Hashtbl.find errors key with
    | exception Not_found ->
        let r = ref [] in
        let msg =
          Printf.sprintf "File %S, line %d: complete function %S for %S."
            "src/ezlibcob/genstubs.ml" line fun_name ty
        in
        Hashtbl.add errors key (msg, r);
        r
    | (_, r) -> r
  in
  if not (List.mem "NOT_IMPLEMENTED" func.fun_attrs) then
    r := func :: !r;
  raise Exit

let print_errors () =
  Hashtbl.iter (fun _ (msg, funcs) ->
      let funcs = !funcs in
      Printf.eprintf "%s\n%!" msg;
      Printf.eprintf "   caused %d functions skipped\n%!"
        (List.length funcs);
      List.iter (fun func ->
          IDL_PARSER.error_msg
            (Printf.sprintf "  * function %S" func.fun_name)
            func.fun_file func.fun_pos;
        ) funcs
    ) errors

(*
let string_of_type (ty, _) = Printf.sprintf "[%s]" ty
let print_func func =
  Printf.eprintf "%s %s(%s);\n"
    (string_of_type (func.fun_return, None))
    func.fun_name
    (String.concat ", "
       (List.map string_of_type func.fun_args))
*)

let ml_type_of_c_type func (ty,argname) =
  let ty =
    match ty with
    | "COB_VA_FIELDN" -> "field array"

    | "const cob_field *"
    | "cob_field *"
      -> "field"
    | "cob_file *"
      -> "file"
    | "cob_decimal *" -> "decimal"
    | "void" -> "unit"
    | "int"
    | "const int"
    | "const char"
    | "const unsigned int"
    | "const size_t"
    | "size_t"
    | "const cob_u32_t"
    | "cob_u32_t"
    | "cob_s32_t"
    | "cob_u16_t"
      -> "int"

    | "const cob_u64_t"
    | "cob_u64_t"
    | "const cob_s64_t"
    | "cob_s64_t"
      -> "int64"
    | "const unsigned char *"
    | "unsigned char *"
    | "const char *"
    | "char *"
      -> "string"
    | "const void *"
    | "void *"
      -> "voids"
    | _ ->
        error func "ml_type_of_c_type" __LINE__ ty
  in
  match argname with
  | None -> ty
  | Some argname ->
      Printf.sprintf "%s:%s"
        (match argname with
           "val" -> "val_" (* keywords cannot be labels... *)
         | _ -> argname
        ) ty

let val_needs_context func ty =
  match ty with
  | "cob_decimal *"
    -> true

  | "COB_VA_FIELDN"
  | "const cob_field *"
  | "cob_file *"
  | "cob_field *"
  | "const cob_s64_t"
  | "cob_s64_t"
  | "const cob_u64_t"
  | "cob_u64_t"
  | "const unsigned char *"
  | "unsigned char *"
  | "const char *"
  | "char *"
  | "const void *"
  | "void *"
  | "void"
  | "int"
  | "const int"
  | "const char"
  | "const unsigned int"
  | "size_t"
  | "const size_t"
  | "const cob_u32_t"
  | "cob_u32_t"
  | "cob_s32_t"
  | "cob_u16_t"
    -> false
  | _ -> error func "needs_context" __LINE__ ty

let is_memory_value func ty =
  match ty with
  | "COB_VA_FIELDN"
  | "cob_field *"
  | "const cob_field *"
  | "const cob_s64_t"
  | "cob_s64_t"
  | "const cob_u64_t"
  | "cob_u64_t"
  | "const unsigned char *"
  | "unsigned char *"
  | "const char *"
  | "char *"
  | "const void *"
  | "void *"
  | "cob_file *"
    -> true

  | "cob_decimal *"
  | "void"
  | "int"
  | "const char"
  | "const int"
  | "const unsigned int"
  | "const unsigned long"
  | "const size_t"
  | "size_t"
  | "const cob_u32_t"
  | "cob_u32_t"
  | "cob_s32_t"
  | "cob_u16_t"
    -> false
  | _ -> error func "is_memory_value" __LINE__ ty

let c_of_ml func ty =
  match ty with
  | "const cob_field *"
  | "cob_field *"
    ->
      "ML_COB_FIELD"
  | "cob_file *"
    ->
      "ML_COB_FILE"
  | "cob_decimal *" -> "ML_COB_DECIMAL"
  | "const cob_s64_t"
  | "cob_s64_t"
  | "const cob_u64_t"
  | "cob_u64_t"
    -> "Int64_val"
  | "const char *"
  | "const unsigned char *"
    -> "String_val"
  | "unsigned char *"
  | "char *"
    -> "(char*)String_val"
  | "const void *"
  | "void *"
    -> "ML_COB_VOIDS"
  | "int"
  | "const int"
  | "const char"
  | "const unsigned int"
  | "const unsigned long"
  | "const size_t"
  | "size_t"
  | "const cob_u32_t"
  | "cob_u32_t"
  | "cob_s32_t"
  | "cob_u16_t"
    -> "Long_val"
  | _ -> error func "c_of_ml" __LINE__ ty

let ml_of_c_res func c_buf ty =
  match ty with
  | "void *"
  | "cob_field *" ->
      Printf.bprintf c_buf "  res_v = caml_alloc (1, Abstract_tag);\n";
      Printf.bprintf c_buf "  Field( res_v,0) = (value)res;\n";
  | "void" ->
      Printf.bprintf c_buf "  res_v = Val_unit;\n"
  | "cob_s32_t"
  | "int" ->
      Printf.bprintf c_buf "  res_v = Val_long( res );\n"
  | "const cob_s64_t"
  | "cob_s64_t"
  | "const cob_u64_t"
  | "cob_u64_t"
    ->
      Printf.bprintf c_buf "  res_v = caml_copy_int64( res );\n"
  | "const char *"
  | "char *" ->
      Printf.bprintf c_buf "  res_v = caml_copy_string( res );\n"
  | _ -> error func "ml_of_c_res" __LINE__ ty

let gen_func func =

  let ml_buf = Buffer.create 10000 in
  let c_buf = Buffer.create 10000 in

  let c_fun_name = Printf.sprintf "ml_%s" func.fun_name in

  let nparams = ref 0 in
  let needs_context = ref false in
  List.iter (fun (ty,_) ->
      if is_memory_value func ty then incr nparams;
      if val_needs_context func ty then needs_context := true
    ) func.fun_args;
  let needs_context = !needs_context in
  if needs_context then incr nparams;
  let nparams = !nparams in
  let is_memory_function =
    nparams > 0 || is_memory_value func func.fun_return
  in
  let nargs = List.length func.fun_args in

  (* Generate the ML signature of the stub *)

  Printf.bprintf ml_buf "external %s : " func.fun_name ;
  if needs_context then Printf.bprintf ml_buf "context -> ";
  Printf.bprintf ml_buf "%s"
    (String.concat " -> "
       (List.map (ml_type_of_c_type func)
          ((match func.fun_args with
                [] -> [ "void", None ]
              | v -> v
            ) @ [ func.fun_return, None ])));

  Printf.bprintf ml_buf " = ";
  if nargs > 5 then
    Printf.bprintf ml_buf "\"%s_bytecode\" " c_fun_name;
  Printf.bprintf ml_buf "\"%s\"%s\n" c_fun_name
    (if is_memory_function then
       ""
     else
       " [@@noalloc]"
    )
  ;

  (* Generate the C stub *)

  begin
    match func with
    | { fun_return = "cob_field *";
        fun_args = [ "COB_VA_FIELDN", _ ];
        _ } ->

        Printf.bprintf c_buf {|
value %s (value fields_v){
  return ml_cob_call_COB_VA_FIELDN (&%s, fields_v);
}
|} c_fun_name func.fun_cname;

    | { fun_return = "cob_field *";
        fun_args = [
          "const int", _;
          "const int", _;
          "COB_VA_FIELDN", _ ];
        _ } ->

        Printf.bprintf c_buf {|
value %s (value arg0_v, value arg1_v, value fields_v){
  return ml_cob_call_int2_COB_VA_FIELDN (&%s, arg0_v, arg1_v, fields_v);
}
|} c_fun_name func.fun_cname;

    | _ ->

        (* print_func func; *)
        (* We will need names for all arguments, even anonymous in the
           prototype *)
        let args = List.mapi (fun i arg ->
            match arg with
              (ty,
               ( None
               | Some "value"
               )
              ) -> (ty, Printf.sprintf "arg%d" i)
            | (ty, Some argname) -> (ty, argname)) func.fun_args
        in

        (* The prototype... *)
        Printf.bprintf c_buf "value %s (" c_fun_name;
        if needs_context then
          Printf.bprintf c_buf "value context_v, ";
        Printf.bprintf c_buf "%s)\n{\n"
          (String.concat ", "
             (List.map (fun (_ty, argname) ->
                  Printf.sprintf "value %s_v" argname
                ) args))
        ;

        (* The CAMLparam line... *)
        if is_memory_function then begin
          Printf.bprintf c_buf "  CAMLparam%d( " nparams;
          let needs_comma = ref false in
          if needs_context then begin
            Printf.bprintf c_buf "context_v";
            needs_comma := true ;
          end;
          List.iter (fun (ty,argname) ->
              if is_memory_value func ty then begin
                if !needs_comma then
                  Printf.bprintf c_buf ", "
                else
                  needs_comma := true;
                Printf.bprintf c_buf "%s_v" argname
              end
            ) args;
          Printf.bprintf c_buf ");\n";
        end;

        (* The CAMLlocal line... *)
        if is_memory_value func func.fun_return then
          Printf.bprintf c_buf "  CAMLlocal1( res_v );\n"
        else
          Printf.bprintf c_buf "  value res_v;\n";

        (* We add provide a context even if most stubs don't need it... *)
        if needs_context then
          Printf.bprintf c_buf
            "  struct ml_context *cs = ML_CONTEXT( context_v );\n";

        (* Let's extract C values from ML values *)
        List.iter (fun (ty, argname) ->
            Printf.bprintf c_buf "  %s %s = %s (%s_v);\n"
              ty argname (c_of_ml func ty) argname;
          ) args ;

        (* Now, call the function with the C values *)
        Printf.bprintf c_buf "  ";
        if func.fun_return <> "void" then
          Printf.bprintf c_buf "%s res = " func.fun_return ;
        Printf.bprintf c_buf "%s (%s);\n"
          func.fun_cname
          (String.concat ", "
             (List.map (fun (_, argname) -> argname) args));

        (* Finally, let's translate the result back to ML *)
        ml_of_c_res func c_buf func.fun_return;

        (* The CAMLreturn line... *)
        if is_memory_function then
          Printf.bprintf c_buf "  CAMLreturn( res_v );\n"
        else
          Printf.bprintf c_buf "  return res_v;\n";
        Printf.bprintf c_buf "}\n\n";

  end;

  (Buffer.contents ml_buf, Buffer.contents c_buf)

let add_file file buf suffix ext =
  let c_file_base = Filename.chop_suffix file ext in
  let c_file_header = c_file_base ^ suffix ^ ext in
  if Sys.file_exists c_file_header then begin
    let contents = EzFile.read_file c_file_header in
    Buffer.add_string buf contents
  end

let () =
  let idl_file = Sys.argv.(1) in
  let funcs = IDL_PARSER.parse_file idl_file in

  let ml_file = Sys.argv.(2) in
  let c_file = Sys.argv.(3) in

  let ml_buf = Buffer.create 10000 in
  Printf.bprintf ml_buf "(* generated by genstubs.ml *)\n";

  let c_buf = Buffer.create 10000 in
  Printf.bprintf c_buf "/* generated by genstubs.ml */\n";

  add_file ml_file ml_buf "_header" ".ml";
  add_file c_file c_buf "_header" ".c";

  let nfuncs = ref 0 in
  let nfailed = ref 0 in
  List.iter (fun func ->
      incr nfuncs ;
      try
        let (ml_str, c_str) = gen_func func in
        Buffer.add_string ml_buf ml_str ;
        Buffer.add_string c_buf c_str ;
      with Exit ->
        incr nfailed
    ) funcs;

  add_file ml_file ml_buf "_trailer" ".ml";
  add_file c_file c_buf "_trailer" ".c";

  EzFile.write_file ml_file (Buffer.contents ml_buf);
  EzFile.write_file c_file (Buffer.contents c_buf);

  Printf.eprintf "%d/%d stubs created\n%!"
    (!nfuncs - !nfailed) !nfuncs;

  print_errors ();
