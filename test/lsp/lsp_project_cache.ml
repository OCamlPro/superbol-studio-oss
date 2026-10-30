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


(** Tests for the persistent representation of a project.

    What is checked here is that a cached project can be serialised {e without}
    [Marshal.Closures], and comes back intact.  A payload carrying code pointers
    is only valid for the byte-identical executable, and reading one from any
    other build is undefined behaviour rather than a miss --- which is why the
    COBOL configuration is cached as the name of its dialect and rebuilt on the
    way back in.

    The dialect is left at its default throughout, so that these tests do not
    depend on which GnuCOBOL configuration files happen to be installed. *)

open Lsp_testing

let layout = Superbol_project.Layout.default

let toml =
  "[cobol]\n\
   source-format = \"free\"\n\
   copyexts = [\"cpy\", \"CPY\"]\n"

let describe project =
  let config = LSP.Project.config project in
  Printf.sprintf "dialect %s, format %s, copyexts %s"
    (Cobol_config.DIALECT.to_string @@ Cobol_config.dialect config.cobol_config)
    (match config.source_format with
     | Cobol_config.Auto -> "auto"
     | Cobol_config.SF _ -> "fixed by the project")
    (String.concat "," config.libexts)

let%expect_test "a cached project carries no functional value" =
  let { projdir; end_with_postproc }, _ = make_lsp_project ~toml () in
  let cached = LSP.Project.to_cache (LSP.Project.in_existing_dir projdir ~layout) in
  (* Serialising without [Closures] raises as soon as anything reachable is a
     functional value, which is exactly what this asserts against. *)
  begin match Marshal.to_string cached [] with
    | _ -> print_endline "serialised with no closures"
    | exception Invalid_argument msg -> Printf.printf "refused: %s\n" msg
  end;
  end_with_postproc "";
  [%expect {| serialised with no closures |}];;

let%expect_test "a project survives the round trip" =
  let { projdir; end_with_postproc }, _ = make_lsp_project ~toml () in
  let project = LSP.Project.in_existing_dir projdir ~layout in
  Printf.printf "before: %s\n" (describe project);
  let restored =
    LSP.Project.of_cache ~rootdir:(LSP.Project.rootdir project) ~layout @@
    Marshal.from_string (Marshal.to_string (LSP.Project.to_cache project) []) 0
  in
  Printf.printf "after:  %s\n" (describe restored);
  end_with_postproc "";
  [%expect {|
    before: dialect default, format fixed by the project, copyexts cpy,CPY
    after:  dialect default, format fixed by the project, copyexts cpy,CPY |}];;
