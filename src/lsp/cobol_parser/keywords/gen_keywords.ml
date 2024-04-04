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

let cmlyname = ref None
let external_tokens = ref ""

let usage_msg = Fmt.str "%s [OPTIONS] file.cmly" Sys.argv.(0)
let anon str = match !cmlyname with
  | None -> cmlyname := Some str
  | Some _ -> raise @@ Arg.Bad "Only one anonymous argument may be given"

let () =
  Arg.parse
    Arg.[
      ("--external-tokens", Set_string external_tokens,
       "<module> Import token type definition from <module>");
    ]
    anon usage_msg

let cmlyname = match !cmlyname with
  | None | Some "" -> Fmt.epr "%s@." usage_msg; exit 1
  | Some s -> s

(* --- *)

include MenhirSdk.Cmly_read.Read (struct let filename = cmlyname end)

let tokens_module = match !external_tokens with
  | "" -> String.capitalize_ascii @@ Filename.basename Grammar.basename
  | s -> s

let emit_prelude ppf =
  List.iter begin fun a ->
    if Attribute.has_label "header" a ||
       Attribute.has_label "keyword.header" a then
      Format.fprintf ppf "%s\n" (Attribute.payload a)
  end Grammar.attributes

let punct attrs =
  (* Note: we reuse the symbol attribute used by the printer. *)
  List.find_opt (Attribute.has_label "symbol") attrs |>
  Option.map Attribute.payload

let keyword attrs =
  List.find_opt (Attribute.has_label "keyword") attrs |>
  Option.map Attribute.payload

let silenced attrs =
  List.find_opt (Attribute.has_label "keyword.silenced") attrs |>
  Option.map Attribute.payload

let intrinsic attrs =
  List.find_opt (Attribute.has_label "keyword.intrinsic") attrs |>
  Option.map Attribute.payload

let pp_terminal ppf t =
  Print.terminal ppf t;
  match Terminal.typ t with
  | None -> ()
  | Some "string" -> Fmt.string ppf " \"\""
  | Some "int" -> Fmt.string ppf " 0"
  | Some t -> Fmt.failwith "unsupported token type: %s" t

let emit_entry attribute_payload ?(comment_token = false) ppf t =
  let start_token ppf = if comment_token then Fmt.string ppf "(*"
  and end_token   ppf = if comment_token then Fmt.string ppf "*)" in
  match Terminal.kind t with
  | `ERROR | `EOF | `PSEUDO ->
      ()                                                            (* ignore *)
  | `REGULAR ->
      match attribute_payload (Terminal.attributes t) with
      | None ->
          ()
      | Some payload when String.trim payload = "" ->         (* auto-generate *)
          Fmt.pf ppf "@\n\"%s\"%t, %a%t;"
            (String.map (function '_' -> '-' | c -> c) (Terminal.name t))
            start_token pp_terminal t end_token
      | Some payload ->
          List.iter
            (fun kwd -> Fmt.pf ppf "@\n%s%t, %a%t;" (String.trim kwd)
                start_token pp_terminal t end_token)
            (String.split_on_char ',' payload)

let emit_keywords_list ppf =
  Fmt.pf ppf "@[<2>let keywords = %s.[" tokens_module;
  Terminal.iter (emit_entry keyword ~comment_token:false ppf);
  Fmt.pf ppf "@]@\n]@."

let emit_puncts_list ppf =
  Fmt.pf ppf "@[<2>let puncts = %s.[" tokens_module;
  Terminal.iter (emit_entry punct ~comment_token:false ppf);
  Fmt.pf ppf "@]@\n]@."

let emit_silenced_keywords_list ppf =
  Fmt.pf ppf "@[<2>let silenced_keywords = %s.[" tokens_module;
  Terminal.iter (emit_entry silenced ~comment_token:true ppf);
  Fmt.pf ppf "@]@\n]@."

let emit_intrinsic_functions_list ppf =
  Fmt.pf ppf "@[<2>let intrinsic_functions = %s.[" tokens_module;
  Terminal.iter (emit_entry intrinsic ppf);
  Fmt.pf ppf "@]@\n]@."

let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\n[@@@@@@warning \"-33\"] (* <- do not warn on unused opens *)\
     @\n%t\
     @\n%t\
     @\n%t\
     @\n%t\
     @\n%t\
     @\n"
    cmlyname
    emit_prelude
    emit_keywords_list
    emit_intrinsic_functions_list
    emit_puncts_list
    emit_silenced_keywords_list

let () =
  emit Fmt.stdout
