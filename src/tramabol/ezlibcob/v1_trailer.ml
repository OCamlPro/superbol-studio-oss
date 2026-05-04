#2 "src/ezlibcob/v1_trailer.ml"
  (*
  type field_kind =
    | FieldSize of int
    | FieldRecord of memfield list ref

  type memfield = {
    field : field ;
    kind : field_kind ;
  }

  type memzone = {
    mutable buffer : buffer ;
    mutable fields : (field * field option) list ;
    mutable closed : bool ;
  }

  let memzone_create () =
    {
      buffer = buffer_create 1;
      fields = [];
      closed = false ;
    }
  let memzone_field memzone ~size field_attr =
    assert (not memzone.closed);
    let buf_pos = memzone.pos in
    let field = field_create ~size memzone.buffer
        ~buf_pos field_attr in
    memzone.pos <- buf_pos + size;
    memzone.fields <- (field, buf_pos) :: memzone.fields;
    field
     *)
end

(* open LIBCOB.TYPES *)
(*
let ml_introspect_field field =
  let size = LIBCOB.field_get_size field in
  let buffer = LIBCOB.field_get_buffer field in
  let attr = LIBCOB.field_get_attr field in

  let buffer_size = LIBCOB.buffer_get_size buffer in
  let buffer_string = LIBCOB.buffer_get_string buffer in
  let buffer_addr = LIBCOB.buffer_get_addr buffer in

  let ty = LIBCOB.field_attr_get_ty attr in
  let digits = LIBCOB.field_attr_get_digits attr in
  let scale = LIBCOB.field_attr_get_scale attr in
  let flags = LIBCOB.field_attr_get_flags attr in
  let pic = LIBCOB.field_attr_get_pic attr in

  Printf.eprintf "field = {\n";
  Printf.eprintf "   size = %d; \n" size;
  Printf.eprintf "   buffer = {\n";
  Printf.eprintf "      string = %S; \n" buffer_string;
  Printf.eprintf "      addr = 0x%04X; \n" buffer_addr;
  Printf.eprintf "      size = %d; \n" buffer_size;
  Printf.eprintf "   }\n";
  Printf.eprintf "   attr = {\n";
  Printf.eprintf "      ty = %s; \n" ( LIBCOB.string_of_ty ty );
  Printf.eprintf "      digits = %d; \n" digits;
  Printf.eprintf "      scale = %d; \n" scale;
  Printf.eprintf "      flags = %d; \n" flags;
  Printf.eprintf "      pic = %s; \n" (match pic with
      | None -> "NULL"
      | Some s -> Printf.sprintf "%S" s);
  Printf.eprintf "   }\n";
  Printf.eprintf "}\n%!";
  ()

let () = Callback.register "ml_introspect_field" ml_introspect_field


let ignore_int (_n : int) = ()

let () =
  Superbol_plugins.Cobcrun.run_file :=
    (fun file ->
       LIBCOB.init [| "main" |];
       ignore_int @@ LIBCOB.resolve_cobol_and_call file;
       LIBCOB.stop_run ~status: 0)
    *)
