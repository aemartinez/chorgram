(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
type t = set setmap
type f = t
let repr m =
  let find x = try Some (Set.Map.find m x) with Not_found -> None in
  let rec repr x =
    match find x with None -> x | Some y -> 
    match find y with None -> y | Some z -> 
	Set.Map.replace m x z; repr z
  in repr
let states m n = 
  let t = Set.Map.create (2*n) in
  Set.Map.iter (fun p q -> 
    Set.Map.replace t p ();
    Set.Map.replace t q ();
  ) m;
  Set.Map.length t
let create = Set.Map.create
let unify m p q =
  let p = repr m p in
  let q = repr m q in
  match Set.compare p q with
    | 0 -> true 
    | 1 -> Set.Map.replace m p q; false
    | _ -> Set.Map.replace m q p; false
