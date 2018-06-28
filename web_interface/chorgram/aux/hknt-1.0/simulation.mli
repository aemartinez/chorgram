(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common

(* type of relations which should be contained in language inclusion *)
type t

(* print the simulation relation *)
val print: out_channel -> t -> unit

(* largest simulation for a NFA *)
val largest: nfa -> t
val largest_naive: nfa -> t
val largest_focs95: nfa -> t

(* take a NFA with two starting sets, compute similarity and return
   the corresponding saturated NFA ans sets *)
val saturate: set*set*nfa -> set*set*nfa
