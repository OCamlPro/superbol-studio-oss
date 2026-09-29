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

open Data_types
open Cobol_common.Srcloc.INFIX

let size: record -> Data_memory.size = fun r ->
  Data_item.size ~&(r.record_item)

let storage: record -> data_storage = fun r ->
  r.record_storage

(* Collections *)

module M = struct
  type t = record
  let compare a b = String.compare a.record_name b.record_name
  let pp ppf r =
    Pretty.print ppf "@[{@;record-name:@;%S,@;storage:@;%a@;}@]" r.record_name
      Data_printer.pp_data_storage r.record_storage
end

module SET = struct
  include Stdlib.Set.Make (M)
  let pp ppf s =
    Pretty.list ~fopen:"@[{" ~fclose:"@]}" ~fempty:"{}" M.pp ppf
      (elements s)
end

module MAP = struct
  include Stdlib.Map.Make (M)
  let pp ?(fbind: _ format = "@ =>@ ") ppv ppf m =
    Pretty.list ~fopen:"@[{" ~fclose:"@]}" ~fempty:"{}" begin fun ppf (k, v) ->
      Pretty.print ppf "@[%a%(%)%a@]" M.pp k fbind ppv v
    end ppf (bindings m)
end
