(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common

type f = (set*set) list
type t = f ref
let rec repr skipped changed p = function
  | [] -> if changed then repr [] false p skipped else p
  | (l,r) as lr::rules -> 
    if Set.subseteq l p then 
	let pr = Set.union p r in
	repr skipped (changed || not (Set.equal p pr)) pr rules
    else
	repr (lr::skipped) changed p rules
let repr rules p = repr [] false p rules
let create _ = ref []
let unify r p q =
  let rules = !r in
  let p = repr rules p in
  let q = repr rules q in
  if Set.equal p q then true else 
    let pq = Set.union p q in
    if Set.equal p pq then r := (q,pq)::rules
    else if Set.equal q pq then r := (p,pq)::rules
    else r := (p,pq)::(q,pq)::rules;
    false

