(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* FIFO queues give breadth-first traversal *)
module BFS = struct
  (* TODO: check Okasaki's book... *)
  type 'a t = 'a list * 'a list
  let empty = [],[]
  let push (h,k) x = h,x::k
  let pop (h,k) = match h with
    | x::h -> Some (x, (h,k))
    | [] -> match List.rev k with 
        | [] -> None
        | x::h -> Some (x, (h,[]))
  let filter f = 
    let rec xfilter last = function
      | [] -> last
      | x::q when f x -> x::xfilter last q
      | _::q -> xfilter last q
    in
    fun (h,k) -> xfilter (Common.filter_rev f k) h,[]
  let fold f (h,k) a =  Common.fold f h (Common.fold f k a)
  let fold_vars vars f =
    let rec aux i x = if i=vars then x else aux (i+1) (f i x) in
    aux 0 
end

(* LIFO queues give depth-first traversal *)
module DFS = struct
  type 'a t = 'a list
  let empty = []
  let push r x = x :: r
  let pop r = match r with
    | [] -> None
    | x::q -> Some (x,q)
  let filter = List.filter
  let fold = Common.fold
  let fold_vars vars f =
    let rec aux i x = if i = -1 then x else aux (i-1) (f i x) in
    aux (vars-1)
end

(* Random queues for random traversal *)
module RFS = struct
  include DFS
  let rec get acc = function
    | _,[] -> None
    | 0,x::q -> Some (x,List.rev_append acc q)
    | n,x::q -> get (x::acc) (n-1,q)
  let pop r = get [] (Random.int (List.length r),r)
end
