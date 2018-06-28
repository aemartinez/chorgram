(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* several implementations for finite sets of integers *)

module type PRE = sig
  type t
  val empty: t
  val union: t -> t -> t
  val inter: t -> t -> t
  val singleton: int -> t
  val mem: int -> t -> bool
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val full: int -> t
  val hash: t -> int
  val fold: (int -> 'a -> 'a) -> t -> 'a -> 'a
  val shift: int -> t -> t
  val size: t -> int
  val rem: int -> t -> t
end

module type T = sig
  include PRE
  val add: int -> t -> t
  val is_empty: t -> bool
  val intersect: t -> t -> bool
  val diff: t -> t -> t
  val subseteq: t -> t -> bool
  val set_compare: t -> t -> [`Lt|`Eq|`Gt|`N]
  val map: (int -> int) -> t -> t
  val iter: (int -> unit) -> t -> unit
  val filter: (int -> bool) -> t -> t
  val forall: t -> (int -> bool) -> bool
  val exists: t -> (int -> bool) -> bool
  val to_list: t -> int list
  val of_list: int list -> t
  val print: out_channel -> t -> unit
  val random: int -> float -> t
  module Map: Hashtbl.S with type key = t
end

module Extend(M: PRE): T = struct
  include M
  let is_empty = equal empty
  let add x = union (singleton x)
  let subseteq x y = equal (union x y) y
  let intersect x y = not (is_empty (inter x y))
  let diff x y = fold rem y x
  let set_compare x y =
    if equal x y then `Eq else
      let xy=union x y in
      if equal xy y then `Lt
      else if equal xy x then `Gt
      else `N
  let of_list = List.fold_left (fun x i -> add i x) empty 
  let to_list x = fold (fun i q -> i::q) x []
  let filter f x = fold (fun i y -> if f i then add i y else y) x empty
  let iter f x = fold (fun i () -> f i) x ()
  let map f x = fold (fun i -> add (f i)) x empty
  let rec xprint f = function
    | [] -> output_char f '}'
    | [x] ->  Printf.fprintf f "%i}" x
    | x::q -> Printf.fprintf f "%i,%a" x xprint q
  let print f x = Printf.fprintf f "{%a" xprint (to_list x)
  let random n p =
    let rec aux acc = function
      | 0 -> acc
      | n ->
	if Random.float 1. < p then 
	  aux (add (n-1) acc) (n-1)
	else aux acc (n-1)
    in aux empty n
  let forall x f =
    try fold (fun i _ -> f i || raise Not_found) x true
    with Not_found -> false
  let exists x f =
    try fold (fun i _ -> f i && raise Not_found) x false
    with Not_found -> true
  module Map = Hashtbl.Make(M)
end

module OList = Extend(struct 
  type t = int list
  let empty = []
  let rec union h k = match h,k with
    | [],l | l,[] -> l
    | x::h', y::k' -> 
      match compare x y with
	| 1 -> y::union h  k'
	| 0 -> x::union h' k'
	| _ -> x::union h' k
  let rec inter h k = match h,k with
    | [],_ | _,[] -> []
    | x::h', y::k' -> 
      match compare x y with
	| 1 ->    inter h  k'
	| 0 -> x::inter h' k'
	| _ ->    inter h' k
  let singleton x = [x]
  let equal = (=)
  let compare = compare
  let rec mem x = function
    | [] -> false
    | y::q -> match compare x y with
	| 1 -> mem x q
	| 0 -> true 
	| _ -> false
  let fold = List.fold_right
  let shift n = List.map ((+) n)
  let hash = Hashtbl.hash
  let rem x h = failwith "todo: OList.rem"
  let rec full acc = function
    | 0 -> acc
    | n -> full (n-1::acc) (n-1)
  let full = full []
  let size = List.length
end)

module Narith = struct 
  type t = int list
(*
0  -> [1]
1  -> [2]
2  -> [4]
...
n  -> [1 lsl n]
...
62 -> [2^62]
63 -> [0; 1]
...
*)
  let empty = []
  let (<::) x q = if x=0 && q=[] then [] else x::q
  let rec union x y = match x,y with 
    | [],z | z,[] -> z
    | a::x, b::y -> a lor b :: union x y
  let rec inter x y = match x,y with 
    | [],_ | _,[] -> []
    | a::x, b::y -> a land b <:: inter x y
  let rec pad acc = function 0 -> acc | n -> pad (0::acc) (n-1)
  let rec get n x = match n,x with 0,a::_ -> Some a | _,[] -> None | n,_::x -> get (n-1) x
  let width = Sys.word_size - 1 
  let singleton i = pad [1 lsl (i mod width)] (i/width)  
  let equal = (=)
  let compare = compare
  let mem i x = match get (i/width) x with
    | None -> false
    | Some a -> (a lsr (i mod width)) land 1 <> 0 
  let shift n x = 
    if x=[] then [] else
    let m = n mod width in
    if m=0 then pad x (n/width) else
    let wm = width-m in 
    let rec xshift b = function
      | [] -> if b=0 then [] else [b]
      | a::x -> b lor (a lsl m) :: xshift (a lsr wm) x
    in
    pad (xshift 0 x) (n/width)
  (* let shift n x =  *)
  (*   pad x (1+n/width) *)
  let fold f x acc =
    let rec aux i acc = function
      | [] -> acc
      | a::x -> aux' i width acc x a
    and aux' i w acc x = function
      | 0 -> aux (i+w) acc x
      | a -> aux' (i+1) (w-1) (if a land 1 <> 0 then f i acc else acc) x (a lsr 1)
    in aux 0 acc x
  let iter f x = fold (fun i () -> f i) x ()
  let hash = Hashtbl.hash
  let full n = 
    let rec xfull acc = function 0 -> acc | n -> xfull (-1::acc) (n-1) in
    let m = n mod width in
    if m=0 then xfull [] (n/width) else
    xfull [1 lsl m - 1] (n/width)
  let rec diff x y = match x,y with 
    | [],_ -> []
    | _,[] -> x
    | a::x, b::y -> a land (lnot b) <:: diff x y
  let size x = fold (fun _ i -> i+1) x 0

  let is_empty = function [] -> true | _ -> false
  let add x = union (singleton x)       (* optimisable *)
  let rec subseteq x y = match x,y with
    | [],_ -> true
    | _,[] -> false
    | a::x, b::y -> (a lor b = b) && subseteq x y
  let rec intersect x y = match x,y with
    | [],_ | _,[] -> false
    | a::x, b::y -> (a land b != 0) || intersect x y
  let rec set_compare x y = match x,y with
    | [],[] -> `Eq
    | [],_ -> `Lt
    | _,[] -> `Gt
    | a::x, b::y -> 
      if a=b then set_compare x y else 
        let c = a lor b in 
        if (c = b) then if subseteq x y then `Lt else `N
        else if (c = a) && subseteq y x then `Gt else `N
  let of_list = List.fold_left (fun x i -> add i x) empty 
  let to_list x = List.rev (fold (fun i q -> i::q) x [])
  let map f x = fold (fun i -> add (f i)) x empty
  let filter f x = fold (fun i x -> if f i then add i x else x) x empty (* optimisable *)
  let rem i = filter (fun j -> i<>j)    (* highly optimisable *)
  let rec xprint f = function
    | [] -> output_char f '}'
    | [x] ->  Printf.fprintf f "%i}" x
    | x::q -> Printf.fprintf f "%i,%a" x xprint q
  let print f x = Printf.fprintf f "{%a" xprint (to_list x)
  let random n p =                      (* optimisable *)
    let rec aux acc = function
      | 0 -> acc
      | n ->
	if Random.float 1. < p then 
	  aux (add (n-1) acc) (n-1)
	else aux acc (n-1)
    in aux empty n
  let forall x f =
    try fold (fun i _ -> f i || raise Not_found) x true
    with Not_found -> false
  let exists x f =
    try fold (fun i _ -> f i && raise Not_found) x false
    with Not_found -> true
  module Map = Hashtbl.Make(struct 
    type t = int list let equal = (=) let compare = compare let hash = hash 
  end)
end


module AVL = Extend(struct
  include Set.Make(struct type t = int let compare = compare end)
  let rem = remove
  let hash = Hashtbl.hash
  let rec full acc = function
    | 0 -> acc
    | n -> full (add (n-1) acc) (n-1)
  let full = full empty
  let size x = fold (fun _ n -> n + 1) x 0
  let shift n x = fold (fun i -> add (i+n)) x empty
end)

