(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
type t = (set*set,unit) Hashtbl.t
type f = t
let create = Hashtbl.create
let unify m x y =
  if Hashtbl.mem m (x,y) then true
  else (Hashtbl.add m (x,y) (); false)
let states m n = 
  let t = Set.Map.create (2*n) in
  Hashtbl.iter (fun (x,y) () -> 
    Set.Map.replace t x ();    
    Set.Map.replace t y ()) m;
  Set.Map.length t
