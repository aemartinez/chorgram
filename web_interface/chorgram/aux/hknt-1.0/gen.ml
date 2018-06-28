(*******************************************************************)
(*  This is part of HKC, it is distributed under the terms of the  *)
(*         GNU Lesser General Public License version 3             *)
(*              (see file LICENSE for more details)                *)
(*                                                                 *)
(*               Copyright 2011-2012: Damien Pous.                 *)
(*******************************************************************)

open Common
open NFA

module Make(UpTo: UPTO)(Queue: QUEUE) = struct
  let equiv (x,y,a) =
    let candidate = UpTo.create a.size in
    let vars = vars a in
    let push_span x y =
      Queue.fold_vars vars (fun v todo -> 
	Queue.push todo (delta_set a v x, delta_set a v y))
    in
    let rec loop n todo = match Queue.pop todo with
      | None -> true,n
      | Some ((x,y), todo) ->
	if Set.intersect a.accept x <> Set.intersect a.accept y then
	  false, n
	else if UpTo.unify candidate x y then 
	  (* note that we don't exploit [todo] with this generic code *)
	  loop n todo
	else 
	  loop (n+1) (push_span x y todo)
    in
      loop 0 (Queue.push Queue.empty (x,y))
  let included (x,y,a) = equiv (Set.union x y,y,a)
end

