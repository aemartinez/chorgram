(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open NFA

(* redundant representation *)
type t = {
  rel: bool array array; (* rel.(i).(j) holds if i<=j *)
  dn: set array; (* [dn.(i)] is the set of states smaller than [i] (except [i]) *)
}

let print f r = Array.iteri (fun i s -> if not (Set.is_empty s) then
    Printf.fprintf f "%a <= %i\n" Set.print s i) r.dn

let exists' x f =
  try Array.fold_right (fun i _ -> f i && raise Not_found) x false
  with Not_found -> true

let iter n f =
  let rec aux n = if n<0 then () else (f n; aux (n-1)) in
  aux (n-1)

let forall n f =
  let rec aux n = n<0 || (f n && aux (n-1)) in
  aux (n-1)

let of_fun n f =
  let rec line i j = 
    if i=n then Set.empty 
    else if i<>j && f i j then Set.add i (line (i+1) j)
    else line (i+1) j
  in
  Array.init n (line 0)

let of_rel n rel = {
  rel=rel;
  dn=of_fun n (fun i j -> rel.(i).(j)) }


(* very naive algorithm to compute the largest simulation *)

let largest_naive a =
  let n = a.size in
  let rel = 
    Array.init n (fun i -> 
      Array.init n (fun j -> 
        Set.mem i a.accept <= Set.mem j a.accept)) 
  in
  let rec loop () =
    let changed = ref false in
    for i=0 to n-1 do
      for j=0 to n-1 do
        if rel.(i).(j) && 
          exists' a.delta (fun d ->
          Set.exists d.(i) (fun i' -> 
          Set.forall d.(j) (fun j' -> 
          not rel.(i').(j'))))
        then (
          changed := true;
          rel.(i).(j) <- false
        )  
      done
    done;
    if !changed then loop() 
    else of_rel n rel 
  in loop()


(* algorithm proposed by M. Henzinger, T. Henzinger, P. Kopke in
   "Computing Simulations on Finite and Infinite Graphs", 
   in Proc. FOCS'95 *)

exception Found of int
let largest_focs95 a =
  let _,a' = Misc.reverse_nfa (Set.empty,a) in
  let pst = delta a in
  let pre = delta a' in
  let pre_set = delta_set a' in
  let size = a.size in
  let all_states = Set.full size in
  let vars = vars a in
  let prevsim = Array.make size all_states in
  let sim = 
    Array.init size (fun v -> 
      Set.filter (fun u ->
        forall vars (fun i -> 
          (Set.is_empty (pst i v)) || not (Set.is_empty (pst i u)))
      ) (if Set.mem v a.accept then a.accept else all_states))
  in
  let rec loop n =
    try iter size (fun v -> if sim.(v) <> prevsim.(v) then raise (Found v))
    with Found(v) ->
      let psv = prevsim.(v) in
      prevsim.(v) <- sim.(v);
      iter vars (fun i ->
        let rem = Set.diff (pre_set i psv) (pre_set i sim.(v)) in
        Set.iter (fun u -> 
          sim.(u) <- Set.diff sim.(u) rem
        ) (pre i v));
      loop (n+1)
  in 
  loop 0;
  let rel = 
    Array.init size (fun u -> 
      Array.init size (fun v -> 
        Set.mem v sim.(u)))
  in
  iter size (fun u -> sim.(u) <- Set.rem u sim.(u));
  { rel; dn=of_fun size (fun i j -> rel.(i).(j)) }



let saturate_set r x = 
  Set.fold (fun i -> Set.union r.dn.(i)) x x

let saturate_nfa r t = 
  { t with delta = Array.map (Array.map (saturate_set r)) t.delta }


(* selecting the implementation of [largest] *)
let largest = largest_focs95


let saturate (i,j,a) = 
  let t = largest a in
  (saturate_set t i, saturate_set t j, saturate_nfa t a)
