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

type symbol = {
    sym_kind : string ;
    sym_name : string ;
    sym_implem : bool ;
    sym_pos : GoblintCil.location ;
  }

let errors = Hashtbl.create 111

let mk_error sym fun_name line ty =
  let ty = GoblintCil.(Pretty.sprint ~width:999999 (Cil.d_type () ty)) in
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
  if sym.sym_implem then
    r := sym :: !r;
  raise Exit

let error_msg (loc : GoblintCil.location) msg =
  Printf.eprintf "src/ezlibcob/%s:%d:%d: error: %s\n%!"
    loc.file loc.line loc.column msg

let print_errors () =
  Hashtbl.iter (fun _ (msg, syms) ->
      let syms = !syms in
      Printf.eprintf "%s\n%!" msg;
      Printf.eprintf "   caused %d symbols skipped\n%!"
        (List.length syms);
      List.iter (fun (sym : symbol) ->
          error_msg sym.sym_pos
            (Printf.sprintf "  * %s %S" sym.sym_kind sym.sym_name);
        ) syms
    ) errors
