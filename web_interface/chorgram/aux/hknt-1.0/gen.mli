(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common

(* equivalence check for sets of states in an NFA, using bisimulations up to *)

module Make(UpTo: UPTO)(Strategy: QUEUE): sig
  val equiv: set*set*nfa -> bool*int
  val included: set*set*nfa -> bool*int
  (* return the result and the size of the candidate *)
end
