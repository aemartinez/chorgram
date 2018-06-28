(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open NFA


(* bridge with libvata *)

let vata = "~/git/libvata/build/cli/vata"

let save (s,i,a) =
  (* let i,a = Misc.reverse_nfa (i,a) in *)
  (* let i,a = a.accept, {a with accept=i} in *)
  let r,f = Filename.open_temp_file ~mode:[Open_creat;Open_text;Open_wronly] "aut" "" in
  Timbuk.print f (s,i,a);
  close_out f;
  r

let vata_included_files ~opt a1 a2 =
  try
    let i = Printf.kprintf Unix.open_process_in "%s -t %s incl %s %s" vata opt a1 a2 in
    let res = Scanf.fscanf i "%i\n%i\n%f\n%i\n" (fun p _ t r -> ((r=1,p),t)) in
    if Unix.close_process_in i = Unix.WEXITED 0 then res
    else raise Not_found
  with e ->
    let command s = print_endline s; ignore (Sys.command s) in
    Printf.printf "ERROR, replaying: ";
    Printf.kprintf command "%s -t %s incl %s %s" vata opt a1 a2;
    flush stdout;
    failwith (match e with 
      | Not_found -> "vata exited with an error" 
      | _ -> "ill-formed vata output")
    
let vata_included ~opt (i,j,a) =
  let a1 = save ("a1",i,a) in
  let a2 = save ("a2",j,a) in
  let res = vata_included_files opt a1 a2 in
  Sys.remove a1;
  Sys.remove a2;
  res

let vata_equivalent ~opt (i,j,a) =
  let a1 = save ("a1",i,a) in
  let a2 = save ("a2",j,a) in
  let (r,p),t as res = vata_included_files ~opt a1 a2 in
  let res = 
    if not r then res else
      let (r',p'),t' = vata_included_files ~opt a2 a1 in
      (r',p+p'),t+.t'
  in
  Sys.remove a1;
  Sys.remove a2;
  res
