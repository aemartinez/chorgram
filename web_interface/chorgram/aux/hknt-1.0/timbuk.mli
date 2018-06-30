(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common

(* print an automaton in timbuk format *)
val print: out_channel -> string*set*nfa -> unit

(* read a word automaton from a file in timbuk format *)
val read_file: ?rev:bool -> string -> set*nfa
