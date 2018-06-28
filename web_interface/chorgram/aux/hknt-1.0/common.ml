(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* select the implementation of sets *)

(* module Set = Sets.OList *)
module Set = Sets.Narith
(* module Set = Sets.AVL *)

type set = Set.t
type 'a setmap = 'a Set.Map.t

(* shortcuts for comparison types *)
type le = [`Lt|`Eq|`Le]
type ge = [`Gt|`Eq|`Ge]

(* abstract signature for candidates with up-to techniques *)
module type UPTO = sig
  (* the argument of create is the number of states *)
  type t
  val create: int -> t
  val unify: t -> set -> set -> bool
end

(* variables (or letters) are just integers *)
type var = int

(* abstract exploration strategy *)
module type QUEUE = sig
  type 'a t
  val empty: 'a t
  val push: 'a t -> 'a -> 'a t
  val pop: 'a t -> ('a * 'a t) option
  val filter: ('a -> bool) -> 'a t -> 'a t
  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_vars: var -> (var -> 'a -> 'a) -> 'a -> 'a
end

(* type for NFA *)
module NFA = struct
  type t = {
    size: int;
    delta: set array array;
    (* delta.(a).(i) is the set of successors of state [i] along letter [a] *)
    accept: set }
  let vars a = Array.length a.delta
  let delta a v x = a.delta.(v).(x)
  let delta_set a v =
    let d = a.delta.(v) in 
    fun x -> Set.fold (fun i -> Set.union d.(i)) x Set.empty
end
type nfa = NFA.t

(* timing a function call *)
let time ?(gc=true) f x =
  if gc then Gc.compact();
  let t0 = Sys.time() in
  let r = f x in
  r,Sys.time()-.t0

(* raising failures using printf *)
let fail fmt = Printf.kprintf failwith fmt

(* checking an equivalence using a generic inclusion test *)
let equivalent_from_i included (a,i,j) = 
  let r,n = included (a,i,j) in
  if r then let r,n' = included (a,j,j) in r,n+n'
  else r,n
          
(* checking an inclusion using a generic equivalence test *)
let included_from_e equivalent (a,i,j) = equivalent (a,Set.union i j,j)

(* filtering and reversing a list *)
let filter_rev f =
  let rec xfilter_rev acc = function
    | [] -> acc
    | x::q when f x -> xfilter_rev (x::acc) q
    | _::q -> xfilter_rev acc q
  in xfilter_rev []

(* tail-rec [List.fold_left] with the type of [List.fold_right] *)
let rec fold f l a = match l with [] -> a | x::q -> fold f q (f x a)
