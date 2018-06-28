(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open NFA

(* timbuk *)

let print f (s,i,a) =
  Printf.fprintf f "Ops ";
  for v=0 to vars a-1 do
    Printf.fprintf f "a%i:1 " v;
  done;
  Printf.fprintf f "x:0\n\nAutomaton %s\n\nStates " s;
  for i=0 to a.size-1 do
    Printf.fprintf f "q%i " i;
  done;
  Printf.fprintf f "\n\nFinal States ";
  Set.fold (fun i () -> Printf.fprintf f "q%i " i) a.accept ();
  Printf.fprintf f "\n\nTransitions\n";
  Set.fold (fun i () -> Printf.fprintf f "x -> q%i\n" i) i ();
  Array.iteri (fun v ->
    Array.iteri (fun i x ->
      Set.fold (fun j () -> 
        Printf.fprintf f "a%i(q%i) -> q%i\n" v i j) x ();
    )
  ) a.delta

let rec filter_map f = function
  | [] -> []
  | x::q -> match f x with 
      | None -> filter_map f q 
      | Some y -> y::filter_map f q

let rec list_idx x = function
  | [] -> fail "undeclared letter: %s" x
  | y::q -> if x=y then 0 else 1+list_idx x q

let read ?(rev=false) f = 
  let alphabet,_,states,accepting,transitions = 
    Parser.nfa Lexer.token (Lexing.from_channel f) in
  let start_letter = 
    match List.filter (fun (_,n) -> n=0) alphabet 
    with [(a,_)] -> a 
      | _ -> fail "could not find the starting letter, not a wor automaton ?"
  in
  let letters = filter_map (function
    | _,0 -> None
    | a,1 -> Some a
    | a,_ -> fail "not a word automaton: the letter %s has arity greater than 1" a
  ) alphabet 
  in
  let size = ref 0 in
  let map = Hashtbl.create (List.length states) in
  List.iter (fun s -> Hashtbl.add map s !size; incr size) states;
  let idx x = 
    try Hashtbl.find map x with 
	Not_found -> fail "undeclared state: %s" x 
  in
  let o = List.map idx accepting in
  let i = filter_map (function
    | (a,None,x) when a=start_letter -> Some (idx x)
    | _ -> None
  ) transitions
  in
  let t = filter_map (function
    | (a,Some x,x') -> Some (idx x,list_idx a letters,idx x')
    | _ -> None
  ) transitions
  in 
  if rev then Set.of_list o, Misc.nfa_of_lists 
    (List.map (fun (i,a,j) -> j,a,i) t) i
  else Set.of_list i, Misc.nfa_of_lists t o
  
let read_file ?rev f =
  let i = open_in f in
  try let x = read ?rev i in close_in i; x 
  with e -> close_in i; Printf.printf "error when parsing %s:\n" f; raise e
