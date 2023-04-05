(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1

module Json_encoding = struct

  include Json_encoding

  exception DestructError

  let destruct encoding buf =
  try
    let json = Ezjsonm.from_string buf in
    destruct encoding json
  with
  | Cannot_destruct (path, exn)  ->
    Format.eprintf "Error during destruction path %a : %s\n%!"
      (Json_query.print_path_as_json_path ~wildcards:true) path
      (Printexc.to_string exn)
    ;
    raise DestructError
  | Unexpected_field field ->
    Format.eprintf "Error during destruction path, unexpected field %S\n%!"
      field ;
    raise DestructError

end

(* If you delete or rename this file, you should add
   'src/package-json/main.ml' to the 'skip' field in "drom.toml" *)

let () =
  let read = ref false in
  Arg.parse [] (fun file ->
      read := true;
      let s = EzFile.read_file file in
      match Json_encoding.destruct Manifest.vscode_enc s with
      | exception Json_encoding.DestructError ->
        Printf.eprintf "File %S parsing failed\n%!" file
      | _ ->
        Printf.eprintf "File %S parsed\n%!" file
    ) "package-json [FILES]: parse files or generate file" ;

  if not !read then
    let p = Json_encoding.construct Manifest.vscode_enc Project.p in
    let s = Ezjsonm.value_to_string ~minify:false p in
    Printf.printf "%s\n%!" s
