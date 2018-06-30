(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* abstract signature for finite sets of natural numbers *)

module type T = sig
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

  (* [random n p] returns a set whose elements are stricly 
     lesser than [n], and appear with a probability [p]  *)
  val random: int -> float -> t

  module Map: Hashtbl.S with type key = t
end

(* sets as ordered lists *)
module OList: T

(* sets as large integers *)
module Narith: T

(* sets as binary balanced trees (i.e., OCaml stdlib) *)
module AVL: T

