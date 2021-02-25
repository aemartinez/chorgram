%%                                        %%
% Authors: Claudio Mezzina & Emilio Tuosto %
%%                                        %%

% ------ Some Top-Level info ----------
%
% Assumed eM_encoding for the GlobalGraph
% -------------------------------------
%
% A list of tuples of the form {line_number, cmd} where
%
% cmd ::=
%      |  {com, A, B, M}                    --    A->B:M                               // A sends to B the message M
%      |  {cho, A, {Gl,Phil}, {Gr,Phir}}    --    A:Gl unless Phil + Gr unless Phir    // A is the active part. for the choice between Gl and Gr
%         // to be generalised to {bra, A, [{G1, Phi1}, {G2, Phi2}, ...]}
%      |  {par, [G1, G2, ...]}              --    G1 | G2 | ...                        // threads must have disjoint participants
%      |  {rec, A, {G, Phi}}                --    repeat A G unless Phi                // A is the ptp controlling the loop
%

%%% TODO: implement actual distribution
%%% TODO: extend syntax with guards on loops and reflect this in the Haskell GG-to-Erlang compiler
%%% TODO: implement actual guards
%%% TODO: design/implement more expressive projectable guards

-module(gg2erl).

-import(aux,
	[
	 mk_send/4,
	 mk_recv/4,
	 mk_list/1,
	 mk_register/5,
	 mk_table/2,
	 mk_tuple/1,
	 mk_comment/1,
	 mk_funName/3,
	 mk_tab/1,
	 tab/0,
	 resettab/0,
	 testtab/0,
	 inctab/0,
	 dectab/0,
	 inc_indent/0,
	 dec_indent/0,
	 indent_dec/0,
	 extractPtps/1,
	 generatePreambleStd/3,
	 stopwatch/1,
	 decorate/3,
	 decorate/4,
	 cpPtps/1,
	 str_cat/2,
	 ins_comma/2,
	 debug/2
	]
).

% Functions used for compilation
-export(
   [
    % creates the body of all the actors at branches
    mk_cp_beh/4,
    % generates all the codec
    mk_all/3
   ]
).



% Creates the functions for each actor


% Base case
mk_cp_beh(_, [], L, _) ->
    {"", L}
;
% Communication
mk_cp_beh(ModuleName, [ {_,{com,_,_,_}} | T ], L, CPs) ->
    mk_cp_beh(ModuleName, T, L, CPs)
;

% Iteration
mk_cp_beh(ModuleName, [ {I, {rec, _A, {G, _}}} | T ], L, CPs) ->
    CP = integer_to_list(I),
    P = maps:get(I,CPs),
    {Nest, _L} = mk_cp_beh(ModuleName, G, L, CPs),
    {Cont, Lf} = mk_cp_beh(ModuleName, T, _L, CPs),
    Mkname = fun(A) -> "loop_" ++ atom_to_list(A) ++ "_" ++ CP end,
    Loop = fun(A) ->
		   Code = mk_comment(atom_to_list(A) ++ " at control point " ++ CP)
		       ++ tab() ++ Mkname(A) ++ "() ->"
		       ++ inc_indent()
		   %% if active, A should spawn the selector of the loop
		       ++ prj_act(ModuleName, G, A, CPs) ++ ",\n" 
		       ++ tab() ++ "receive"
		       ++ inc_indent()
		       ++ mk_tuple([CP, "loopBack"]) ++ " -> " ++ Mkname(A) ++ "();"
		       ++ dec_indent()
		       ++ mk_tuple([CP, "exitLoop"]) ++ " -> " ++ "exitLoop"
		       ++ dec_indent()
		       ++ "end."
		       ++ resettab(),
		   Code
	  end,
    {lists:foldr(fun(A,X) ->  X ++ Loop(A) end, "", P) ++ Nest ++ Cont, Lf ++ lists:map(fun(X) -> Mkname(X) ++ "/0" end, P)}
;
% Fork
mk_cp_beh(ModuleName, [ {_,{par,Gs}} | T ], L, CPs) ->
    F = fun(G, {X, Y}) ->
		{C, Lf} = mk_cp_beh(ModuleName, G, Y, CPs),
		{X ++ C, Lf}
	end,
    {C, _L} = lists:foldr(F, {"", L}, Gs),                      % Note: the fold won't work for non-Erlang projections
    {D, Lf} = mk_cp_beh(ModuleName, T, _L, CPs),
    {C ++ D, Lf}
;

%% More general branch
		   
mk_cp_beh(ModuleName, [ {I, {bra, _A, Branch}} | T ], L, CPs) ->
    CP = integer_to_list(I),
    {Branches, Lr} = lists:foldr(fun({B, _}, {R, L1}) ->
					 { Bcode, LB } = mk_cp_beh(ModuleName, B, L1, CPs),
					 { R ++ Bcode, LB }
				 end,
				 {"", L},
				 Branch
				),
    {Cont, Lf} = mk_cp_beh(ModuleName, T, Lr, CPs),
    Aux = fun(A, X) ->
		  Clause = fun({{G, _}, N}) ->
				   case lists:member(A, extractPtps(G)) of
				       true -> inc_indent()
						   ++ mk_tuple([CP, "branch_" ++ integer_to_list(N)]) ++ " ->"
						   ++ inc_indent()
						   ++ prj_act(ModuleName, G, A, CPs)
						   ++ dectab()
						   ++ dectab();
				       _    -> ""
				   end
			   end,
		  Ptp = atom_to_list(A),
		  Actor = str_cat(["act", Ptp, CP], "_"),
		  Code = mk_comment(Ptp ++ " at the branching " ++ CP)   % The function for the participant
		      ++ resettab()
		      ++ Actor ++ "() ->"
		      ++ inc_indent()
		      ++ "receive"
		      ++ str_cat(lists:map(Clause, lists:zip(Branch, lists:seq(1, length(Branch)))), ";")
		      ++ dec_indent()
		      ++ "end.",
		  {Code ++ X,  [Actor ++ "/0"]}
	  end,
    {C, E} = lists:foldr(
	       fun(A, {X, Y}) ->
		       {CodeA, LA} = Aux(A, X),
		       {CodeA, LA ++ Y}
	       end,
	       {Cont, Lf},
	       maps:get(I, CPs)
	      ),
    {Branches ++ C, E}
;

% Branch
mk_cp_beh(ModuleName, [ {I, {cho, _A, {Gl, _}, {Gr, _}}} | T ], L, CPs) ->
    CP = integer_to_list(I),
    {BranchL, Ll} = mk_cp_beh(ModuleName, Gl, L, CPs),
    {BranchR, Lr} = mk_cp_beh(ModuleName, Gr, Ll, CPs),
    {Cont, Lf} = mk_cp_beh(ModuleName, T, Lr, CPs),
    Aux = fun(A, X) ->
		  resettab(),
		  Ptp = atom_to_list(A),
		  Actor = str_cat(["act", Ptp, CP], "_"),
		  Code = mk_comment(Ptp ++ " at the choice at " ++ CP)       % The function for the participant		      
		      ++ tab() ++ Actor ++ "() ->"
		      ++ inc_indent()
		      ++ "receive"
		      ++ inc_indent()
		      ++ mk_tuple([CP, "left"]) ++ " ->\n"                   % left branch starts here
		      ++ inctab()
		      ++ prj_act(ModuleName, Gl, A, CPs)
		      ++ ";"
		      ++ indent_dec()
		      ++ mk_tuple([CP, "right"]) ++ " ->\n"                   % left branch starts here
		      ++ inctab()
		      ++ prj_act(ModuleName, Gr, A, CPs)
		      ++ dectab()
		      ++ indent_dec()
		      ++ "end.",
		  {Code ++ X,  [Actor ++ "/0"]}
	  end,
    {C, E} =
	lists:foldr(
	  fun(A, {X, Y}) ->
		  {CodeA, LA} = Aux(A, X),
		  {CodeA, LA ++ Y}
	  end,
	  {Cont, Lf},
	  maps:get(I, CPs)
	 ),
    {BranchL ++ BranchR ++ C, E}
.



% ------------------- Code for actors ---------------------

% Base case
prj_act(_, [], _, _) -> ""
;

% Iteration
prj_act(ModuleName, [ {I, {rec, _A, {_,_}}} | T ], A, CPs) ->
    CP = integer_to_list(I),
    Cont = prj_act(ModuleName, T, A, CPs),
    %% if A is the active participant, then it spawns the selector of the loop
    Body = case lists:member(A, maps:get(I, CPs)) of
	       true ->
		   if _A == A  ->
			   %% fetching the # of iterations
			   ParList = mk_list([CP,
					      mk_list(lists:map(fun(X) -> atom_to_list(X) end, maps:get(I, CPs))),
					      maps:get(CP,get(rnd))
					     ]
					    ),
			   "debug(\"[~p]\t Selector started\", [" ++ CP ++ "], 1, " ++ integer_to_list(get(debug)) ++ "),\n"
			       ++ tab() ++ "spawn(" ++ str_cat([ModuleName, "loop_sel", ParList], ", ") ++ "),\n" ++ tab();
		      _A =/= A -> ""
		   end
		       ++ mk_funName("loop", A, CP) ++ "()";
	       _ -> ""
	   end,
    ins_comma(Body, Cont)
;

% Communication: sender case
prj_act(ModuleName, [ {I, {com, A, B, M}} | T ], A, CPs) ->
    C = mk_send(atom_to_list(A), atom_to_list(B), "", [integer_to_list(I), atom_to_list(M)] ),
    Cont = prj_act(ModuleName, T, A, CPs),
    ins_comma(C, Cont)
;

% Communication: receiver case
prj_act(ModuleName, [ {I, {com, A, B, M}} | T ], B, CPs) ->
    C = mk_recv(A, B, "", [integer_to_list(I), atom_to_list(M)] ),
    Cont = prj_act(ModuleName, T, B, CPs),
    ins_comma(C, Cont)
;

% Communication: neither sender nor receiver case
prj_act(ModuleName, [ {_, {com, A, B, _}} | T ], C, CPs)  when A =/= C;B =/= C->
  prj_act(ModuleName, T,C, CPs)
;

% Fork
prj_act(ModuleName, [ {_, {par, Gs}} | T ], A, CPs) ->
    Cont = prj_act(ModuleName, T, A, CPs),
    Thread = lists:foldr(fun(X, B) -> B ++ prj_act(ModuleName, X, A, CPs) end, "", Gs),              % Note: the fold won't work for non-Erlang projections
    ins_comma(Thread, Cont)
;

% Branch
prj_act(ModuleName, [ {I, {bra, _A, _}} | T ], A, CPs) ->
    CP = integer_to_list(I),
    Cont = prj_act(ModuleName, T, A, CPs),
    Ptps = maps:get(I,CPs),
    ParList =  mk_list(["[]",
			CP,
			mk_list(lists:map(fun(X) -> atom_to_list(X) end, maps:get(I, CPs))),
			"fun() -> getBranch(" ++ mk_list([CP, "branching_table"]) ++ ", bra) end"
		       ]
		      ),
    Branch =  if _A == A  -> tab() ++ "spawn(" ++ str_cat([ModuleName, "sel_act", ParList], ", ") ++ "),\n";
		 _A =/= A -> ""
	      end
	      ++ case lists:member(A, Ptps) of
		     true -> tab() ++ mk_funName("act", A, CP) ++ "()";
		     _    -> ""
		 end,
    ins_comma(Branch, Cont)
;

% Choice
prj_act(ModuleName, [ {I, {cho, _A, _, _}} | T ], A, CPs) ->
    CP = integer_to_list(I),
    Cont = prj_act(ModuleName, T, A, CPs),
    Ptps = maps:get(I,CPs),
    ParList =  mk_list(["[]",
			CP,
			mk_list(lists:map(fun(X) -> atom_to_list(X) end, maps:get(I, CPs))),
			"fun () -> getBranch(" ++ mk_list([CP, "fwdOnly", "branching_table"]) ++ ") end"
		       ]
		      ),
    Branch = if _A == A  -> "spawn(" ++ str_cat([ModuleName, "sel_act", ParList], ", ") ++ "),\n" ++ tab();
		_A =/= A -> ""
	     end
	     ++ case lists:member(A, Ptps) of
		    true -> mk_funName("act", A, CP) ++ "()";
		    _    -> ""
		end,
    ins_comma(Branch, Cont)
.

% ------------------------  End section: Code for actors


% --------------------------------------------------Section
% ----------- Assembling all Code together ----------------

mk_all(ModuleName, G, Dict) ->
    %% Set some parameters for the compilation
    %
    put(reporting, maps:get(reporting, Dict)),
    put(debug, maps:get(debug, Dict)),
    put(rnd, maps:get(rnd, Dict)),
    put(indent, 0),

    CPs = cpPtps(G),
    %% Compute functions and participants of each control point 
    %
    {Functions, L} = mk_cp_beh(ModuleName, G, [], CPs),
    Aux = fun(A) -> mk_register(ModuleName, atom_to_list(A), mk_funName("", A, "fun"), "[]", get(debug)) end,
    Processes = lists:usort(extractPtps(G)),
    file:write_file(
      "./" ++ ModuleName ++ ".erl",
      generatePreambleStd(L ++ lists:map(fun(X) -> mk_funName("", X, "fun/0") end, Processes), ModuleName, get(debug))
      ++ Functions
      ++ str_cat(
	   lists:map(
	     fun(A) -> decorate(inctab() ++ inctab() ++ prj_act(ModuleName, G, A, CPs) ++ dectab() ++ dectab(),
				"",
				tab() ++ mk_comment("Coordinating start / end of " ++ atom_to_list(A))
				++ mk_funName("", A, "fun") ++ "() ->"
				++ inc_indent()
				++ "receive {go, Pid} ->"
				++ inc_indent(),
				",\n" ++ tab() ++ "Pid ! done_actor"
				++ indent_dec() ++ "end"
			       )
			   ++ dectab()
	     end, Processes),
	   ".\n"
	  )
      ++ ".\n"
      ++ mk_comment("Let's start...")
      ++ "main() ->"
      ++ inc_indent()
      ++ mk_table("branching_table", get(rnd)) ++ ",\n"
      ++ tab() ++ "ets:insert("++ "branching_table,{debug," ++ integer_to_list(get(debug)) ++"}),\n"
      ++ tab() ++ str_cat(lists:map(Aux, Processes), ",\n" ++ tab()) ++ ",\n"
      ++ tab() ++ stopwatch(bang) ++ "\n"                                           % "run rabbit, run"
      ++ tab() ++ str_cat(lists:map(
			    fun(X) -> atom_to_list(X) ++ " ! {go, self()}" end,
			    Processes
			   ),
			  ",\n" ++ tab()
		)
      ++ ",\n"
      % waiting ack%
      ++ tab() ++ str_cat(lists:map(
			    fun(_) -> "receive done_actor -> ok end" end,
			    Processes
			   ),
			  ",\n" ++ tab()
			 )
      ++ ",\n"
      ++ tab() ++ stopwatch(stop)                                     % "and then one day you find..."
      ++ "\n."
     )
.

% ------------------------  End section: All code
