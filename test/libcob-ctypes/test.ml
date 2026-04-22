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

module COB = EzCob.V1

let () =
  if (COB.Types.version <> 5) then
    failwith "Expected libcob version 5";

  COB.Module.global_init 0 Ctypes.(from_voidp (ptr char) null);
  let cob_module, _globals_ptr =
    COB.Module.init ~name:"foo" ~source:__MODULE__
  in
  COB.Module.enter cob_module;

  let memory = COB.Field.alloc_memory ~size:8 in
  Bigarray.Array1.fill memory ' ';

  let foo =
    COB.Field.(create ~memory ~offset:0 ~size:3
                 ~attrs:(alphanum_attrs ~byte_size:3))
  and bar =
    COB.Field.(create ~memory ~offset:3 ~size:5
                 ~attrs:(numeric_attrs ~digits:5 ~scale:0))
  and bax =
    COB.Field.(create ~memory ~offset:3 ~size:5
                 ~attrs:(alphanum_attrs ~byte_size:5))
  in

  COB.Termio.accept foo;
  COB.Termio.accept bar;
  COB.Termio.display ~newline:true [|foo; bar|] ;
  COB.Termio.display ~newline:true [|bax|] ;
  COB.Field.move foo bax;
  COB.Termio.display ~newline:true [|bax; foo|] ;

  COB.Module.leave cob_module
;;
