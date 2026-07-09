(*
   Copyright (c) 2013
    Gabriel Kerneis     <gabriel@kerneis.info>
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. The names of the contributors may not be used to endorse or promote
   products derived from this software without specific prior written
   permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 *)

open Cil

module E = Errormsg

type t = {
    mutable fd_enabled: bool;
    fd_name: string;
    fd_description: string;
    fd_extraopt: (string * Arg.spec * string) list;
    fd_doit: (file -> unit);
    fd_post_check: bool;
}

let features = ref []

let same_name s f = s = f.fd_name

let register f =
  if List.exists (same_name f.fd_name) !features
  then E.s (E.error "Feature %s is already registered" f.fd_name)
  else features := !features @ [f]

let list_registered () = !features

let find s = List.find (same_name s) !features

let registered s =
  try ignore(find s); true
  with Not_found -> false

let enabled s = try (find s).fd_enabled with Not_found -> false

let enable s =
  try
    let f = find s in f.fd_enabled <- true
  with Not_found ->
    E.s (E.error "cannot enable feature %s: not found" s)
