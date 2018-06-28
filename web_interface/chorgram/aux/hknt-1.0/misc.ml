(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open NFA

let nfa_print_transitions f a =
  Array.iteri (fun v -> Array.iteri (fun i x -> 
    if not (Set.is_empty x) then
      Printf.fprintf f "%i -%i-> %a\n" i v Set.print x;
  )) a.delta
let nfa_print f a =
  nfa_print_transitions f a;
  Printf.fprintf f "accept: %a\n" Set.print a.accept
let nfa_print' f (i,a) =
  Printf.fprintf f "init: %a\n" Set.print i;
  nfa_print f a

let nfa_of_lists delta accept =
  let vars,size =
    let m = List.fold_left max 0 accept in
    List.fold_left (fun (w,k) (i,v,j) -> (max v w, max (max i j) k)) (0,m) delta
  in
  let d = Array.init (vars+1) (fun _ -> Array.make (size+1) Set.empty) in
  List.iter (fun (i,v,j) -> 
    d.(v).(i) <- Set.add j d.(v).(i)
  ) delta;
  {
    size = size+1;
    delta = d;
    accept = Set.of_list accept;
  }

let nfa_union (a_start,a) (b_start,b) =
  let vars = max (vars a) (vars b) in
  let size = a.size+b.size in
  let shift = Set.shift a.size in
  let b_start = shift b_start in
  let delta = Array.init vars (fun _ -> Array.make size Set.empty) in
  Array.iteri (fun v -> Array.iteri (fun i dvi -> delta.(v).(i) <- dvi)) a.delta;
  Array.iteri (fun v -> Array.iteri (fun i dvi -> delta.(v).(a.size+i) <- shift dvi)) b.delta;
  a_start, b_start,
  { size=size;
    delta=delta;
    accept = Set.union a.accept (shift b.accept) }

(* generate a random nfa *)
let random_nfa ~n ~v ~r ~pa =
  let d = r /. float_of_int n in
  { size = n;
    delta = Array.init v (fun _ -> 
      Array.init n (fun _ -> (Set.random n d)));
    accept = Set.random n pa }

(* co-accessible states of an nfa *)
let co_accessible a = 
  let rev = Array.make a.size Set.empty in
  let rec loop x acc =
    Set.fold (fun i acc -> 
      if Set.mem i acc then acc
      else loop rev.(i) (Set.add i acc)
    ) x acc 
  in
  for v=0 to vars a-1 do
    for i=0 to a.size-1 do
      Set.fold (fun j () -> rev.(j) <- Set.add i rev.(j)) a.delta.(v).(i) ()
    done
  done;
  loop a.accept Set.empty 

(* live states of an nfa *)
let live (i,a) = 
  let next x = 
    Array.fold_right (fun d -> 
      Set.fold (fun i -> Set.union d.(i)) x) 
      a.delta Set.empty 
  in
  let coacc = co_accessible a in
  let rec loop x acc =
    let x = Set.inter x coacc in
    if Set.is_empty x then acc
    else 
      let acc = Set.union x acc in 
      loop (Set.diff (next x) acc) acc
  in loop i Set.empty

(* normalise an nfa by removing useless states and reindexing states *)
let normalise_nfa_states (i,j,a) =
  let live = live (Set.union i j,a) in
  let t = Array.of_list (Set.to_list live) in
  let size = Array.length t in
  let t' = Array.make a.size None in
  Array.iteri (fun i j -> t'.(j) <- Some i) t;
  let map x = Set.fold (fun i acc ->
    match t'.(i) with
      | Some j -> Set.add j acc
      | None -> acc
  ) x Set.empty in
  let delta = 
    Array.init (vars a) (fun v ->
      Array.init size (fun i -> 
        map a.delta.(v).(t.(i))))
  in
  map i, map j, { size; delta; accept=map a.accept }

(* normalise an nfa by removing useless variables *)
let normalise_nfa_vars a =
  let non_empty t = 
    try for i=0 to Array.length t-1 do 
        if not (Set.is_empty t.(i)) then raise Not_found
      done; false
    with Not_found -> true
  in
  let delta = Array.of_list (List.filter non_empty (Array.to_list a.delta)) in
  { a with delta }

(* normalised union: the resulting nfa only has live states, and no spurious letters *)
let nfa_normalised_union (ix,x) (iy,y) = 
  let ix,_,x = normalise_nfa_states (ix,Set.empty,x) in
  let iy,_,y = normalise_nfa_states (iy,Set.empty,y) in
  let (i,j,a) = nfa_union (ix,x) (iy,y) in
  (i,j,normalise_nfa_vars a)

(* reversing an NFA *)
let reverse_nfa = 
  let transpose n d =
    let d' = Array.make n Set.empty in
    for i=0 to n-1 do
      Set.fold (fun j () -> 
	d'.(j) <- Set.add i d'.(j)
      ) d.(i) ()
    done;
    d'
  in 
  fun (start,a) -> a.accept, { a with
    accept = start;
    delta = Array.map (transpose a.size) a.delta;
  }
