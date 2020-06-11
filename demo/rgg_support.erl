%%                                        %%
% Authors: Claudio Mezzina & Emilio Tuosto %
%%                                        %%

-module(rgg_support).

-import(lists, [foreach/2]).

%-import(reporting, [debug/0]).

-export([
	 lookup/2,
	 debug/3,
	 debug/4,
	 debugLoop/0,
	 smartReg/3,
	 smartUnreg/1,
	 notify/3,
	 getBranch/1,
	 getBranch/2,
	 loop_sel/6,
	 sel_act/4,
	 sel_mon/3,
%%	 loop_mon/3,
	 loop_entry/6,
	 rb/6,
	 lb/6,
	 el/2
	]).


%% --------------- Auxiliary function ----------------------

start_monitor(CP, P, Fun) ->
    put(debug, get(debug)),
    MonName = list_to_atom("sel_mon_" ++ integer_to_list(CP)),
    Proc = spawn(rgg_support, Fun, [CP, P, self()]),
    smartReg(MonName, Proc, get(debug))
.

lookup(Rnd, CP) ->
    [{_,V}] = lists:filter(fun({CP1,_}) -> CP == CP1 end, Rnd),
    V
.

%% set debugRegistration() lower than debug() if you want debugging messages
debugRegistration() -> 7
.

%% set debugLoop() lower than debug() if you want debugging messages
debugLoop() -> 5
.

%% set debugRev() lower than debug() if you want debugging messages
%% debugRev() -> 5
%% .

%% Note that the debug function behaves as a normal
%% print if Level is strictly negative
debug(Fmt, Params, Level) ->
    debug(Fmt, Params, Level, get(debug))
.

debug(Fmt, Params, Level, Debug) ->
    case Debug >= Level of
	true  -> io:format("::\t " ++ Fmt ++ "\n", Params);
	false -> ""
    end
.

smartReg(Pname, Proc, Debug) ->
    case lists:member(Pname, registered())  of
	true -> debug("smartReg: ~p registered already", [Pname], debugRegistration(), Debug),
		smartReg(Pname, Proc, Debug);
	_    -> register(Pname, Proc)
    end,
    debug("smartReg: ~p started", [Pname], 1, Debug),
    debug("smartReg: now ~p has been registered", [Pname], debugRegistration(), Debug)
.

smartUnreg(Pname) ->
    debug("smartUnreg: Unregistering ~p", [Pname], debugRegistration()),
    case lists:member(Pname, registered()) of
	true -> unregister(Pname);
	_    -> smartUnreg(Pname)
    end,
    debug("smartUnreg: now ~p has been unregistered", [Pname], debugRegistration())
.

notify(CP, M, P) ->
    [{_, DB}] = ets:lookup(branching_table, debug),
    foreach(
      fun(X) ->
	   debug("[~p] \t Selector ! ~p: " ++ atom_to_list(M), [CP, X], 1, DB),
	      X ! {CP, M}
      end,
      P
     )
.

% TODO: getBranch(Param, bra) is used in ice18 only; eventually to be used everywhere
getBranch(Param, bra) ->
    CP    = lists:nth(1, Param),
    Table = lists:nth(2, Param),
    [{_, V}]     = ets:lookup(Table, CP),
    io:format(" ---> ~p\n",[V]),    
    list_to_atom("branch_" ++ integer_to_list(V))
.

getBranch(Param) ->
    case Param of
	[] -> case rand:uniform(2) of
		  1 -> left;
		  2 -> right
	      end;
	_  -> CP       = lists:nth(1, Param),
	      Mode     = lists:nth(2, Param),
	      Table    = lists:nth(3, Param),
	      [{_, V}] = ets:lookup(Table, CP),
	      case V of
		  1 -> case Mode of mimic -> right; _ -> left  end;
		  2 -> case Mode of mimic -> left ; _ -> right end
	      end
    end
.

loop_entry(CP, P, V, RollBack, Mod, LoopMod) ->
    %% waiting aggregated decision from the selector's monitor
    %% note that in the simulation the value of the decision is immaterial
    [{_, Rounds}] = ets:lookup(branching_table, CP),

    
    Itr = Rounds - V,
    %% Get the aggregated outcome from the monitor of the selector
    case Mod of
	mimic ->
	    %% fetching loop modality set by experiments:testLoop
	    case LoopMod of
		stuttering ->
		    receive {CP, _} ->
			    case RollBack > Itr of
				true ->
				    start_monitor(CP, P, sel_mon),
				    lb(CP, P, V, RollBack, Mod, LoopMod);       % The current execution of the loop is at an iteration preceeding the last rollback, hence it should not be roll-backed
				_    -> case {RollBack =/= Rounds, V > 0} of
					    {true,  _    } ->
						start_monitor(CP, P, sel_mon),
						rb(CP, P, Rounds, RollBack, Mod, LoopMod);
					    {false, true } ->
						start_monitor(CP, P, sel_mon),
						lb(CP, P, V, RollBack, Mod, LoopMod);
					    {false, false} -> el(CP, P)
					end
			    end
		    end;
		currentIteration ->
		    %% At the end of the execution in this semantics
		    %% the following equations hold
		    %%   Rounds == number of loopBack == number of rollBack messages
		    receive {CP, _} ->
			    case RollBack == Itr + 1 of
				true -> case V > 0 of
					    true ->
						start_monitor(CP, P, sel_mon),
						lb(CP, P, V, RollBack, Mod, LoopMod);
					    _ -> el(CP, P)
					end;
				_    ->
				    start_monitor(CP, P, sel_mon),
				    rb(CP, P, V, RollBack, Mod, LoopMod)
			    end
		    end;
		entireIteration ->
		    [{_,OV}] = ets:lookup(branching_table, CP),
		    case {V == 0, RollBack} of
			{true, 0} ->
			    %% monitor should be already on: TODO: it shouldn't
			    notify(CP, endIteration, P),
			    receive {CP, _} -> rb(CP, P, OV, RollBack, Mod, LoopMod) end;
			{true, 1} ->
			    start_monitor(CP, P, sel_mon),
			    notify(CP, endIteration, P),
			    receive {CP, _} -> el(CP, P) end;
			{true, _} -> whatTHE;
			{_, _}    -> lb(CP, P, V, RollBack, Mod, LoopMod)
		    end
	    end;
	fwdOnly -> receive {CP, _} ->
			   case V == 0 of
			       true -> el(CP, P);
			       _    -> start_monitor(CP, P, sel_mon),
				       lb(CP, P, V, RollBack, Mod, LoopMod)
			   end
		   end;
	_ -> case V == 0 of
		 true -> el(CP, P);
		 _ -> lb(CP, P, V, RollBack, Mod, LoopMod)
	     end
	     %% It does not make sense to reverse an end-loop if V>0 when using the forward-only semantics
    end
.

rb(CP, P, V, RollBack, Mod, LoopMod) ->
    notify(CP, rollBack, P ),
    loop_entry(CP, P, V, RollBack + 1, Mod, LoopMod)
.

lb(CP, P, V, RollBack, Mod, LoopMod) ->
    case {Mod, LoopMod} of
	{fwdOnly, _} -> notify(CP, loopBack, P);
	{_, entireIteration} -> notify(CP, loopBack, P);
	{_,_} -> notify(CP, loopBack, P)
    end,
    loop_entry(CP, P, V - 1, RollBack, Mod, LoopMod)
.

el(CP, P) ->
    notify(CP, exitLoop, P ),
    debug("exiting ~p", [CP], debugLoop())
.




%% --------------- Actors and monitors ---------------------

loop_sel(CP, P, V, RollBack, Mod, LoopMod) ->
	%before loop_mon
	start_monitor(CP, P, sel_mon),                   % starting monitor
    %%
    %% V >= 0 otherwise the loop is infinite
    %%
    notify(CP, startLoop, P),
    loop_entry(CP, P, V, RollBack, Mod, LoopMod)
.

sel_act(Attempt, CP, P, GetBranch) ->
    start_monitor(CP, P, sel_mon),
    %% determine selection left or right
    Sel =
	case Attempt of
	    [] -> GetBranch();
	    [left] -> right;
	    [right] -> left;
	    _ -> throw("panic: no more branches available. Totally unexpected: this is REALLY ODD!!!")
	end,
    % send the selection to each participant
    notify(CP, Sel, P),
    %% receive the outcome from the Selector Monitor and decide the fate of the branch
    receive {CP, Outcome} ->
	    Decision =
		case {Outcome, Attempt} of
		    {fwd, _}  -> fwd;
		    {rev, []} -> rev;
		    {_,_}     -> fwd
		end
    end,
    notify(CP, Decision, P),
    %% foreach(fun(X) -> debug("[~p] \t Selector ! ~p :~p\n", [CP, X, Sel], 1), X !  {CP, Decision} end, P),
    case Decision of
	rev -> sel_act(Attempt ++ [Sel], CP, P, GetBranch);
	_   -> end_branch
    end
.

sel_mon(CP, Ptps, Selector) ->
    %% receives the messages from all the monitors of the participants
    %% Ptpts. If one of them is to reverse the loop then 'rev' is sent
    %% to 'Selector' Note that this monitor is ephimeral
    %%
    MsgList = lists:map(fun(_) -> receive {CP, M} -> M end end, Ptps),
    Msg = case lists:member(rev, MsgList) of
	      true -> rev;
	      _    -> fwd
	  end,
    debug("[~p] \t Monitor ! Selector: ~p", [CP, Msg], 1, get(debug)),
    Selector!{CP, Msg},
    debug("[~p] \t Monitor done", [CP], 1, get(debug))
.

%% loop_mon(CP, Ptps, Selector) ->
%%     %% As the monitor of the branch, loop_mon collects the local
%%     %% outcomes, aggregates them, and forwards the result to its
%%     %% selector
%%     sel_mon(CP, Ptps, Selector),
%%     receive
%% 	{CP, rollBack} -> debug("loop_mon_~p rolling back", [CP], debugLoop()), sel_mon(CP, Ptps, Selector);
%% 	{CP, loopBack} -> debug("loop_mon_~p looping back", [CP], debugLoop()), sel_mon(CP, Ptps, Selector);
%% 	{CP, exitLoop} -> debug("loop_mon_~p exiting", [CP], debugLoop())
%%     end
%% .
