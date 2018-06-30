(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open NFA

module type CHECKER = sig
  (* how to enqueue elements of the todo list *)
  module Q: QUEUE
  (* checking outputs of the given sets *)
  val check: nfa -> set -> set -> bool
  (* unifying function (i.e., the up-to technique) *)
  val unify: unit -> set -> set -> (set*set) Q.t -> bool
end


module Make(R: CHECKER): sig 
  (* explore the nfa starting from the given sets, using the given checker *)
  val run: set*set*nfa -> bool*int
end = struct
  exception CE
  let run (x,y,t) = 
    let tic = ref 0 in
    let vars = vars t in
    let push_span x y = 
      incr tic;
      R.Q.fold_vars vars (fun v todo ->
        R.Q.push todo (delta_set t v x, delta_set t v y)
      )
    in
    let unify = R.unify() in
    let rec loop todo = 
      match R.Q.pop todo with
        | None -> true
        | Some ((x,y),todo) -> 
	  if not (R.check t x y) then raise CE;
	  if unify x y todo then loop todo 
	  else loop (push_span x y todo)
    in 
    let r = try loop (R.Q.push R.Q.empty (x,y)) with CE -> false
    in r, !tic
end


module type RULES = sig
  (* representing a set of rewriting rules to perform efficient congruence tests.
     this module provides only the "one-pass" rewriting process *)
  type t
  val empty: t
  val add: set -> set -> t -> t	(* adding a rewriting rule to the candidate *)
  val pass: t -> set -> t * set	(* a single parallel rewriting pass *)
end

module type ERULES = sig
  (* more functions on rules, derived from the one-pass rewriting process *)
  type t
  val empty: t
  val add: set -> set -> t -> t
  val norm: t -> set -> set
  val pnorm: t -> set -> set -> (t*set) option
  val norm': ('a -> set -> 'a*set) -> t -> 'a -> set -> set
  val pnorm': ('a -> set -> 'a*set) -> t -> 'a -> set -> set -> (t*set) option
end

module ER(R: RULES): ERULES = struct
  include R

  (* get the normal form of [x] *)
  let rec norm rules x =
    let rules,x' = pass rules x in
    if Set.equal x x' then x else norm rules x'

  (* get the normal form of [y], unless [x] is subsumed by this normal
     form. In the first case, also return the subset of rules that
     were not applied *)
  let pnorm rules x y = 
    let rec pnorm rules y =
      if Set.subseteq x y then None else
	let rules,y' = pass rules y in
	if Set.equal y y' then Some (rules,y') else pnorm rules y'
    in pnorm rules y

  (* get the normal form of [x] w.r.t a relation and a todo list *)
  let norm' f =
    let rec norm' rules todo x =
      let rules,x' = pass rules x in
      let todo,x'' = f todo x' in 
      if Set.equal x x'' then x else norm' rules todo x''
    in norm'

  (* get the normal form of [y] w.r.t a relation, unless [x] is
     subsumed by the normal form of [y] w.r.t a relation and a todo
     list. In the first case, the normal form is only w.r.t the
     relation, and we also return the subset of (relation) rules that
     were not applied. *)
  let pnorm' f rules todo x y =
    let rec pnorm' rules todo y =
      if Set.subseteq x y then true else
	let todo,y' = f todo y in 
	let rules,y'' = pass rules y' in
	if Set.equal y y'' then false else
	  pnorm' rules todo y''
    in
    match pnorm rules x y with
      | None -> None
      | Some(rules,y') as r -> if pnorm' rules todo y' then None else r
end


module SR = ER(struct
  (* simple candidates as lists of pairs, rewritten from left to right *)
  type t = (set*set) list
  let empty = []
  let pass rules x = List.fold_left (fun (rules,z) (x,y as xy) ->
    if Set.subseteq x z then rules, Set.union y z else
      xy::rules,z
  ) ([],x) rules
  let add x y rules = (x,y) :: rules
end)

module LR = ER(struct
  (* candidates as lists of pairs, rewritten from left to right ; 
     the candidate is simplified a little bit when adding new pairs *)
  type t = (set*set) list
  let empty = []
  let pass rules x = List.fold_left (fun (rules,z) (x,y as xy) ->
    if Set.subseteq x z then rules, Set.union y z else
      xy::rules,z
  ) ([],x) rules
  let add x x' =
    let upd z' = if Set.subseteq x z' then Set.union x' z' else z' in
    let rec xadd' = function
      | [] -> []
      | (z,z')::q -> (z,upd z')::xadd' q
    in
    let rec xadd = function
      | [] -> [x,x']
      | (z,z')::q -> match compare x z with
	  | 1 -> (z,upd z')::xadd q
	  | 0 -> (z,Set.union x' z')::xadd' q
	  | _ -> (x,x')::xadd' q
    in
    xadd
end)

module TR = ER(struct
  (* candidates as binary trees, allowing to cut some branches during
     rewriting *)
  type t = L of set | N of (set*t*t)
  let empty = L Set.empty
  let rec xpass skipped t z = match t with
    | L x -> skipped, Set.union x z
    | N(x,tx,fx) -> 
      if Set.subseteq x z then 
	let skipped,z = xpass skipped tx z in 
	xpass skipped fx z
      else
	let skipped,z = xpass skipped fx z in 
	N(x,tx,skipped),z
  let pass = xpass empty
  let add x x' =
    let rec add' = function
    (* optimisable *)
    | L y -> L (Set.union y x') 
    | N(y,t,f) -> N(y,t,add' f)
    in
    let rec add = function
    | L y as t -> 
      if Set.subseteq x y then L (Set.union y x') 
      else N(x,L (Set.union y x'),t)
    | N(y,t,f) -> match Set.set_compare x y with
	| `Eq -> N(y,add' t,f)
	| `Lt -> N(x,N(y,add' t,L x'),f)
	| `Gt -> N(y,add t,f)
	| `N  -> N(y,t,add f)
    in add
end)

(* selecting the implementation of candidates *)
module R = TR


(* HKC algorithm for language equivalence *)
module Equivalence(Q: QUEUE) = Make(struct
  module Q = Q
  let check t x y = Set.intersect t.accept x = Set.intersect t.accept y
  let step todo z = Q.fold (fun (x,y as xy) (todo,z) ->
    if Set.subseteq x z then todo, Set.union y z else
      if Set.subseteq y z then todo, Set.union x z else
	Q.push todo xy,z
  ) todo (Q.empty,z)
  let unify () = 
    let r = ref R.empty in
    fun x y todo -> let r' = !r in 
      match R.pnorm' step r' todo x y, R.pnorm' step r' todo y x with
	| None, None -> true
	| Some(ry,y'), None -> 
	  r := R.add y (R.norm ry (Set.union x y')) r'; false
	| None, Some(rx,x') -> 
	  r := R.add x (R.norm rx (Set.union x' y)) r'; false
	| Some(ry,y'), Some(_,x') -> 
	  let z = R.norm ry (Set.union x' y') in
	  r := R.add x z (R.add y z r'); false
end)

(* HKC specialised for language inclusion *)
module Inclusion(Q: QUEUE) = Make(struct
  module Q = Q
  let check t x y = not (Set.intersect t.accept x) || Set.intersect t.accept y
  let step todo z = Q.fold (fun (x,y as xy) (todo,z) ->
    if Set.subseteq y z then todo, Set.union x z else
      Q.push todo xy,z
  ) todo (Q.empty,z)
  let unify () = 
    let r = ref R.empty in
    fun x y todo -> let r' = !r in 
      match R.pnorm' step r' todo x y with
	| None -> true
	| Some(ry,y') -> r := R.add y (R.norm ry (Set.union x y')) r'; false
end)
