(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open Sets

val nfa_print: out_channel -> nfa -> unit
val nfa_print': out_channel -> set*nfa -> unit
val nfa_print_transitions: out_channel -> nfa -> unit (* only transitions *)

val nfa_of_lists: (int*var*int) list -> int list -> nfa
(* transitions, accepting states *)

val nfa_union: set*nfa -> set*nfa -> set*set*nfa
val nfa_normalised_union: set*nfa -> set*nfa -> set*set*nfa
(* the returned sets correspond to the starting sets of the two
   given NFA *)

val random_nfa: n:int -> v:int -> r:float -> pa:float -> nfa
(* generate a random NFA with
   - [n] nodes, 
   - [v] variables,
   - transition density [r] (expected outdegree of each node, 
     for each variable, cf. Tabakov&Vardi'05)
   - probability [pa] for a state to be accepting. 

   if [disjoint] is set, then the produced automaton is a disjoint
   union of two automata such that 0 and 1 do not belong to the same
   automaton.
*)

val reverse_nfa: set*nfa -> set*nfa
