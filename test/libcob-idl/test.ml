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

module COB = Libcob_idl.V1

let () =
  COB.Module.global_init [| |] ;
  let cob_module, _cob_globals =
    COB.Module.init ~name:"foo" ~source:__MODULE__
  in
  COB.Module.enter cob_module;

  let memory = COB.Field.alloc_memory 8 in
  Bigarray.Array1.fill memory ' ';

  let foo =
    COB.Field.(create ~memory ~offset:0 ~size:3
                 ~attrs:{ type_ = TYPE_ALPHANUMERIC;
                          digits = 0; scale = 0; flags = [];
                          pic = [| { symbol = 'X'; times_repeated = 3 } |] })
  and bar =
    COB.Field.(create ~memory ~offset:3 ~size:5
                 ~attrs:{ type_ = TYPE_NUMERIC;
                          digits = 5; scale = 0; flags = [];
                          pic = [| { symbol = '9'; times_repeated = 5 } |] })
  and bax =
    COB.Field.(create ~memory ~offset:3 ~size:5
                 ~attrs:{ type_ = TYPE_ALPHANUMERIC;
                          digits = 0; scale = 0; flags = [];
                          pic = [| { symbol = 'X'; times_repeated = 5 } |] })
  in

  COB.Termio.accept foo;
  COB.Termio.accept bar;
  COB.Termio.display ~newline:true [|foo; bar|] ;
  COB.Termio.display ~newline:true [|bax|] ;
  COB.Field.move foo bax;
  COB.Termio.display ~newline:true [|bax; foo|] ;

  COB.Module.leave cob_module
;;
