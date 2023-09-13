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

let name = ref ""
let external_tokens = ref ""

let usage = Fmt.str "Usage: %s [OPTIONS] file.cmly" Sys.argv.(0)

let anon str =
  if !name = "" then
    name := str
  else
    raise @@ Arg.Bad usage

let () =
  Arg.parse
    Arg.[
    ("--external-tokens", Set_string external_tokens,
     "<module> Import token type definitions from <module>");
  ]
    anon usage

include MenhirSdk.Cmly_read.Read (struct let filename = !name end)

let tokens_module =
  match !external_tokens with
  | "" -> String.capitalize_ascii @@ Filename.basename Grammar.basename
  | s -> s

let all_ctxts = ref []

let context_list str =
  String.split_on_char ',' str |>
  List.filter_map (fun ctxt ->
      let ctxt = String.trim ctxt in
      if ctxt <> "" then
        Some ctxt
      else
        None)

let has_contexts attrs =
  List.find_opt (Attribute.has_label "contexts") attrs |> Option.is_some

let contexts attrs =
  List.find_opt (Attribute.has_label "contexts") attrs |>
  Option.fold ~none:[] ~some:(fun attr -> context_list @@ Attribute.payload attr)

let emit_contexts ppf t =
  match Terminal.kind t with
  | `ERROR | `EOF | `PSEUDO ->
      ()
  | `REGULAR ->
      let attrs = Terminal.attributes t in
      if has_contexts attrs then
        let contexts = contexts (Terminal.attributes t) in
        Fmt.pf ppf "%s,@ [%a];@\n"
          (Terminal.name t)
          Fmt.(list ~sep:(any ";@ ") string) contexts

let emit_specs ppf =
  Fmt.pf ppf "@[<2>let specs = %s.[@\n" tokens_module;
  (*So they are in alphabetical order*)
  let terminals = ref [] in
  Terminal.iter (fun t -> terminals := t :: !terminals);
  List.iter (emit_contexts ppf) !terminals;
  Fmt.pf ppf "@]]@\nin@\n"

let emit_empty_record ppf =
  Fmt.(list ~sep:(any ";@\n") (fun ppf c -> Fmt.pf ppf "    %s = empty" c) ppf) !all_ctxts

let emit_ctxt_funs ppf =
  Fmt.(list
         ~sep:(any "@\n")
         (fun ppf c ->
            Fmt.pf ppf "let %s t c = { c with %s = add t c.%s } in" c c c)
         ppf)
    !all_ctxts

let emit_fold ppf =
  Fmt.pf ppf "List.fold_left (fun (acc, cstoks, unimpl) (t, add_contexts) ->@\n\
             \  let h = Text_lexer.handle_of_token t in@\n\
             \  List.fold_left (fun acc f -> f h acc) acc add_contexts,@\n\
             \  TH.add h cstoks,@\n\
             \  if add_contexts = [] then TH.add h unimpl else unimpl)@\n\
             (empty, TH.empty, TH.empty) specs"

let emit_tokens_contexts ppf =
  Fmt.pf ppf "let all, sensitive_tokens, sensitive_tokens_unimplemented =@\n\
              @[<2>  let open TH in@\n\
              let empty =@\n\
             \  {@\n\
              %t;@\n\
             \  }@\n\
              in@\n\
              %t@\n\
              %t\
              %t@]"
    emit_empty_record
    emit_ctxt_funs
    emit_specs
    emit_fold

let emit_context_type ppf =
  let ctxts = ref [] in
  Terminal.iter (fun t ->
      let attrs = Terminal.attributes t in
      if has_contexts attrs then
        begin
          ctxts := (contexts attrs) @ !ctxts
        end);
  let ctxts = List.sort_uniq (String.compare) !ctxts in
  all_ctxts := ctxts;
  Fmt.(list ~sep:(any ";@\n") (fun ppf n -> Fmt.pf ppf "    %s: t" n) ppf) ctxts

let emit_prelude ppf =
  Fmt.pf ppf "module TH = Text_lexer.TokenHandles@\n\
              type context = TH.t@\n@\n\
              type t = context@\n\
              type contexts =@\n\
              \  {@\n\
              %t;@\n\
              \  }@\n"
    emit_context_type

let emit_context_values ppf =
  List.iter (fun ctxt ->
      Fmt.pf ppf "let %s = all.%s@\n" ctxt ctxt)
    !all_ctxts

let emit ppf =
  Fmt.pf ppf
    "(* Caution this file was automatically generated from %s; do not edit *)@\n\
     %t@\n\
     %t@\n@\n\
     %t@\n"
    !name
    emit_prelude
    emit_tokens_contexts
    emit_context_values

let () =
  emit Fmt.stdout

