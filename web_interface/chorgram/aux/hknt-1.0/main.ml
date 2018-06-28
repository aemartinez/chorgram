(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

(* simple program exploiting the library *)

open Common
open Misc

(* obtaining the various algorithms *)

(* Hopcroft and Karp's algorithm, 
   using a union-find datastructure (not used) *)
module HK_BFS = Gen.Make(UF)(Queues.BFS)
module HK_DFS = Gen.Make(UF)(Queues.DFS)

(* HKC specialised for language inclustion *)
module HKC_BFS_incl = HKC.Inclusion(Queues.BFS)
module HKC_DFS_incl = HKC.Inclusion(Queues.DFS)

(* HKC *)
module HKC_BFS = HKC.Equivalence(Queues.BFS)
module HKC_DFS = HKC.Equivalence(Queues.DFS)

(* do we use similarity or not *)
let sim = ref false

(* do we reverse the automata or not *)
let rev = ref false

(* do we use the breadth first of depth first heuristic *)
let bfs = ref true

(* running a single instance *)
let run kind f1 f2 =
  let a1 = Timbuk.read_file ~rev:!rev f1 in
  Printf.printf "%s: %i states\n%!" f1 (snd a1).NFA.size;
  let a2 = Timbuk.read_file ~rev:!rev f2 in
  Printf.printf "%s: %i states\n%!" f2 (snd a2).NFA.size;
  let (_,_,a12) as a = nfa_normalised_union a1 a2 in
  Printf.printf "normalised union: %i states\n%!" a12.NFA.size;
  let a = 
    if !sim then 
      let a,t = time Simulation.saturate a in
      Printf.printf "similarity computed in %.2fs\n%!" t;
      a
    else a 
  in
  let f = match !bfs,kind with
    | true,`Incl -> HKC_BFS_incl.run
    | false,`Incl -> HKC_DFS_incl.run
    | true,`Equiv -> HKC_BFS.run
    | false,`Equiv -> HKC_DFS.run
  in
  let (b,n),t = time f a in
  Printf.printf "%s %s %s: %b,\t%.3f seconds,\t%i processed pairs\n%!" 
    f1 (if kind =`Incl then "<=" else "==") f2 b t n

(* parsing command line arguments *)
let rec parse_args = function 
  | [] -> exit 0
  | "-bwd"::q -> rev := true; parse_args q
  | "-fwd"::q -> rev := false; parse_args q
  | "-sim"::q -> sim := true; parse_args q
  | "-nosim"::q -> sim := false; parse_args q
  | "-bfs"::q -> bfs := true; parse_args q
  | "-dfs"::q -> bfs := false; parse_args q
  | "-incl"::f1::f2::q -> run `Incl f1 f2; parse_args q
  | "-equiv"::f1::f2::q -> run `Equiv f1 f2; parse_args q
  | _ -> Printf.printf "usage: %s {[options]* {-incl,-equiv} file1 file2}*
options:
 -fwd / -bwd    process the automata forward (default) or backward
 -bfs / -dfs    use the breadth-first (default) of depth-first exploration strategy
 -sim / -nosim  use similarity or not (default)
" Sys.argv.(0)

(* main entry point *)
let _ = parse_args (List.tl (Array.to_list Sys.argv))
