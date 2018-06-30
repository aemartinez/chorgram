(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* implementation of HKC for language equivalence and specialised to
   language inclusions *)

open Common
open NFA

module Equivalence(Q: QUEUE): sig 
  val run: set*set*nfa -> bool*int
end

module Inclusion(Q: QUEUE): sig 
  val run: set*set*nfa -> bool*int
end
