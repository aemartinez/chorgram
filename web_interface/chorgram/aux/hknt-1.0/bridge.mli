(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* Bridge with the libvata C++ implementation of antichain algorithms 

   libvata must be available, and patched to display the number of
   pairs inserted in the antichain and the required time *)

open Common

(* C++ version of the antichain algorithms (libvata).
   the string argument can be used to pass options) *)
val vata_included: opt:string -> set*set*nfa -> (bool*int)*float
val vata_equivalent: opt:string -> set*set*nfa -> (bool*int)*float

(* same thing, reading the automata in the two given files *)
val vata_included_files: opt:string -> string -> string -> (bool*int)*float

(* path to the vata executable *)
val vata: string

