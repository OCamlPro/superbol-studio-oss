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

open Ez_file.V1

let rec mkdir_rec path =
  if not ( Sys.file_exists path ) then begin
    let dir = Filename.dirname path in
    if dir <> path then mkdir_rec dir ;
    Unix.mkdir path 0o755;
  end

let write_file ?(mkdir=false) file =
  if mkdir then mkdir_rec ( Filename.dirname file );
  EzFile.write_file file
