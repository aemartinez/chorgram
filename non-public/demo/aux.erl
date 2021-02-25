-module(aux).

-export(
   [
    % Debugging
    debug/2,
    % String manipulation for source code generation
    str_cat/2,
    decorate/3,
    decorate/4,
    ins_comma/2,
    prefixing/3,
    prefixing/4,
    % Code Snippet Generation.
    mk_funName/3,
    mk_send/4,
    mk_recv/4,
    mk_tuple/1,
    mk_list/1,
    mk_register/5,
    mk_unregister/1,
    mk_impexp/2,
    mk_table/2,
    mk_tab/1,
    tab/0,
    resettab/0,
    testtab/0,
    inctab/0,
    dectab/0,
    inc_indent/0,
    dec_indent/0,
    indent_dec/0,
    generatePreamble/2,
    generatePreambleStd/3,
    stopwatch/1,
    cpPtps/1,
    my_strip/1,
    mk_comment/1,
    extractPtps/1
   ]
  ).

-import(lists, [usort/1,map/2]).



% -------------------------------------------------
% indentation

mk_tab(N) -> 
    string:copies("   ", abs(N))
.


tab() ->
    mk_tab(get(indent))
.

resettab() ->
    put(indent, 0),
    ""
.

testtab() ->
    "\n" ++ tab() ++ "%testtab \t " ++ integer_to_list(get(indent)) ++ "\n"
.

inctab() ->
    put(indent, get(indent) + 1),
    ""
.


dectab() ->
    case {get(indent) =< 0, get(indent)} of
	{true, _} -> throw("negative tab!! ::: " ++ integer_to_list(get(indent)));
	{_ , _} -> put(indent, get(indent) - 1)
    end,
    ""
.

inc_indent() ->
    inctab(),
    "\n" ++ mk_tab(get(indent))
.


dec_indent() ->
    dectab(),
    "\n" ++ mk_tab(get(indent) + 1)
.


indent_dec() ->
    dectab(),
    "\n" ++ mk_tab(get(indent))
.





% -------------------------------------------------
%
% Debugging/2
%
% Used for generating debug printline messages.
% Inner function debug/0 used to quickly toggle between printing messages and not.


debug(Fmt, Params) ->
    case get(reporting) of
	     true -> io:format("::\t " ++ Fmt ++ "\n", Params);
	     _    -> ok
    end
.

% -------------------------------------------------
%
% Supporting functions for code manipulation
%

% str_cat/2
%
% generates a string representation of a list of strings,
% where every item in the list is delineated by SEP

str_cat(Msgs, Sep)->
    case lists:filter(fun(X) -> X =/= "" end, Msgs) of
	[] -> "";
	[T] -> T;
	[H | T] -> H ++ Sep ++ str_cat(T, Sep)
    end
.

% my_strip/1
%
% remove all blank, tabs, and newlines from S

my_strip(S) ->
    case S of
	[] -> S;
	[$\s | T] -> my_strip(T);
	[$\t | T] -> my_strip(T);
	[$\n | T] -> my_strip(T);
	[C | T]   -> [C] ++ my_strip(T)
    end
.

% prefixing/3
%
% Takes 3 strings, returns a string  depending on whether the first one is "" or not

prefixing(S, SepThen, SepElse) ->
     if
       S == "" -> SepThen;
       S =/= "" -> SepElse ++ S
   end
.

% prefixing/4
%
% Takes 4 strings, returns a string  depending on whether the first one is "" or not

prefixing(S, SepThen, SepElse, Suffix) ->
     if
       S == "" -> SepThen;
       S =/= "" -> SepElse ++ S ++ Suffix
     end
.

% decorate/4
%
% Takes 4 strings, returns the 2nd string if the first is empty,
% otherwise concatenates the 3rd, the 1st, and the 4th string

decorate(S, Empty, Prefix, Suffix) ->
    case my_strip(S) of
       "" -> Empty;
       _  -> Prefix ++ S ++ Suffix
     end
.

% decorate/3
%
% As decorate/4 with an empty suffix

decorate(S, SepThen, SepElse) ->
    decorate(S, SepThen, SepElse, "")
.

% ins_comma/2
%
% inserts ",\n" between two non-empty strings
% otherwise it returns either of the non-empty string

ins_comma(S1, S2) -> case {S1,S2} of
			 {"", _} -> S2;
			 {_, ""} -> S1;
			 _       -> S1 ++ ",\n" ++ S2
		     end
.


% -------------------------------------------------
%
% Making handy code generation for frequent operations
%

mk_funName(Pre, A, Suf) ->
    str_cat([Pre, atom_to_list(A), Suf], "_")
.

% mk_send
%
% Generates Erlang code to:
% 1. produce a printline announcing the output.
% 2. generate the code for the output

mk_send(Sender, Receiver, Suffix, Msgs) ->
    % I : A B ! msg
    [I | M] = Msgs,
    A = Sender,
    B = Receiver,
    case get(reporting) of
	false -> "";
	_ ->
	    tab() ++ "debug(\"[~p]   ~p ! ~p : ~p\", "
		++ mk_list([I, A, B ++ Suffix, mk_list([str_cat(M, ", ")])])
		++ ", 1, "
		++ integer_to_list(get(debug))
		++ "),\n"
    end
	++ tab() ++ B ++ Suffix ++ " ! {" ++ str_cat(Msgs, ", ") ++ "}"
.

mk_recv(Sender, Receiver, Suffix, Msgs) ->
    % I : A B ? msg
    [I | M] = Msgs,
    A = atom_to_list(Sender),
    B = atom_to_list(Receiver),
    tab() ++ "receive {" ++ str_cat(Msgs, ", ") ++ "} ->"
	++ case get(reporting) of
	       false -> " rcv_" ++ I ++ " end";
	       _     -> tab() ++ "debug(\"[~p]   ~p ? ~p : ~p\", "
			    ++ mk_list([I, A, B ++ Suffix, mk_list([str_cat(M, ", ")])]) ++ ", 1, "
			    ++ integer_to_list(get(debug))
			    ++ ")"
			    ++ "end"
	   end
.

mk_tuple(L) ->
    "{" ++ str_cat(L, ", ") ++ "}"
.

% mk_list/1
%
% Returns a string representation of a list of elements L
% every element is delimited by ", "


mk_list(L) ->
    "[" ++ str_cat(L, ", ") ++ "]"
.

% mk_register/5
%
% returns a code snippet to register
% the process ModuleName:Proc/|Params|

mk_register(ModuleName, Id, Proc, Params, Debug) ->
    "smartReg(" ++ Id ++ ", spawn(" ++ str_cat([ModuleName, Proc, Params], ", ") ++ "), " ++ integer_to_list(Debug) ++ ")"
.

mk_unregister(Id) ->
    "unregister(" ++ Id ++ ")"
.


mk_impexp(S, L) ->
    case S of
	"export" -> "-export([" ++ str_cat(L,",\n\t") ++ "]).";
	_        -> "-import(" ++ S ++ ",\n\t[" ++ str_cat(L,",\n\t") ++ "])."
    end
.


mk_table(N, D) ->
    S = "case lists:member(" ++ N ++ ", ets:all()) of"
	++ inc_indent()
	++ "true -> ets:delete(" ++ N ++ ");\n"
	++ tab() ++ "_ -> noTable"
	++ indent_dec()
	++ "end,\n"
	++ tab() ++ "ets:new(" ++ N ++ ", [set, named_table, public])",
    Aux = fun(K) ->
		  "ets:insert(" ++ N ++ ", "
		      ++ mk_tuple([K, maps:get(K, D)]) ++ ")"
	  end,
    lists:foldr(fun(X, Acc) -> Acc ++ ",\n" ++ tab() ++ Aux(X) end, S, maps:keys(D))
.



mk_comment(C) ->
    "\n\n%% ----------------------------------------\n%% " ++ C ++ "\n\n"
.


% List of functions imported from rgg_support
rgg_support_imp() -> ["smartReg/3",
		      "smartUnreg/1",
		      "while/3",
		      "notify/3",
		      "loop_sel/6",
		      "sel_act/4",
		      "sel_mon/3",
		      "debug/4",
		      "getBranch/1",
		      "debugLoop/0",
		      "lookup/2"
		     ]
.

% -------------------------------------------------
%
% Preamble code
%

% RevMode \in {random, fwdOnly, revOnly, guard, mimic}



generatePreamble(L, ModuleName) ->
    "-module(" ++ ModuleName ++ ").\n\n"
	++ mk_comment("Import helper functions")
	++ mk_impexp("export", L ++ ["main/0"])
	++ mk_comment("The code to chose a branch...trivial for the moment")
	++ mk_impexp("rgg_support", rgg_support_imp())
	++ "\n\n" ++
	mk_comment("change procInfo to return process_info(self()) for non-experiments")
	++ "procInfo() -> process_info_self\n."
	++ mk_comment("checkGuard is also very trivial!")
	++ "checkGuard(Phi, Info) -> checkGuard(Phi, Info, revOnly).\n\n"
	++ mk_comment("Eventually only checkGuard/3 should be used")
	++ "checkGuard(Phi, Info, Mode) ->"
	++ inc_indent()
	++ "case Mode of"
	++ inc_indent()
	++ "random  ->                           % random check for test only"
	++ inc_indent()
	++ "case rand:uniform(2) of"
	++ inc_indent()
	++ "1 -> true;\n"
	++ tab()
	++ "2 -> false"
	++ indent_dec()
	++ "end;"
	++ indent_dec()
	++ "fwdOnly -> false;                    % this should behave according to the standard semantics"
	++ "\n" ++ tab()
	++ "revOnly -> true;                     % this reverses as much as possible all the branches"
	++ "\n" ++ tab()
	++ "mimic   -> true;                     % this takes the dual branch taken in the standard semantics and reverses them"
	++ "\n" ++ tab()
	++ "std     -> false;"
	++ "\n" ++ tab()
	++ "guard   -> Phi(Info)"
	++ indent_dec()
	++ "end."
	++ inc_indent()
.


generatePreambleStd(L, ModuleName, Debug) ->
    resettab() ++ "-module(" ++ ModuleName ++ ").\n\n"
	++ mk_comment("Import helper functions")
	++ mk_impexp("export", L ++ ["main/0", "sel_act/4", "loop_sel/3"])
	++ mk_comment("The code to chose a branch...trivial for the moment")
	++ mk_impexp("rgg_support", ["notify/3", "getBranch/2", "getBranch/1", "debug/4", "debugLoop/0", "lookup/2", "smartReg/3"]) ++ "\n\n\n\n"
	++ "sel_act(_, CP, P, Branch) ->\n"
	++ "%% choose one of the branches"
	++ inc_indent() ++ "Sel = Branch(),\n"
	++ tab() ++ "%% send the selection to each participant\n"
	++ tab() ++ "notify(CP, Sel, P).\n\n"
	++ indent_dec()
	++ "loop_sel(CP, P, V) ->"
	++ inc_indent()
	++ "case V of"
	++ inc_indent()
	++ "0 ->"
	++ inc_indent()
	++ "lists:foreach(fun(X) -> X ! {CP, exitLoop} end, P),\n"
	++ tab() ++ "rgg_support:debug(\"exiting loop ~p\", [CP], rgg_support:debugLoop(), " ++ integer_to_list(Debug) ++ ");"
	++ indent_dec()
	++ "_ ->"
	++ inc_indent()
	++ "lists:foreach(fun(X) -> rgg_support:debug(\"[~p] \t ~p ! loopBack\", [CP, X], 1, " ++ integer_to_list(Debug) ++ "), X ! {CP, loopBack} end, P),\n"
	++ tab()
	++ "rgg_support:debug(\"looping back ~p. V = ~p\", [CP, V], rgg_support:debugLoop(), " ++ integer_to_list(Debug) ++ "),\n"
	++ tab()
	++ "loop_sel(CP, P, V - 1)"
	++ dectab()
	++ indent_dec()
	++ "end.\n\n"
	++ resettab()
.


stopwatch(Signal) ->
    case Signal of
	bang ->
	    "STime = os:system_time(),";
	stop ->
	    %% "{_, CTime} = statistics(runtime),\n" ++ tab() ++
	    "file:write_file(\".time.tmp\", integer_to_list(os:system_time() - STime))";
	_    -> erlang:error(bad_signal)
    end
.


% -------------------------------------------------
%
% Code for extractP(L)
%

% Functions leading to extractP(L),
% that extracts a list of participants from a Global graph.

extractPtps([]) -> []
;
extractPtps([{_, {com,A,B,_}} | T ]) -> [A,B] ++ extractPtps(T)
;
extractPtps([{_, {cho, _A, {G,_}, {F,_}}} | T ]) -> [_A] ++ extractPtps(G) ++ extractPtps(F) ++ extractPtps(T)
;
extractPtps([{_, {bra, _A, []}} | T ]) -> [_A] ++ extractPtps(T)
;
extractPtps([{I, {bra, _A, [{G, _} | Branch]}} | T ]) -> extractPtps(G) ++ extractPtps([{I, {bra, _A, Branch}} | T ])
;
extractPtps([{_, {par, Gs}} | T ]) -> lists:foldr(fun(Thread,L) -> L ++ extractPtps(Thread) end, [], Gs) ++ extractPtps(T)
;
extractPtps([{_, {rec, A, {G, _}}} | T ]) ->  [A] ++ extractPtps(G) ++ extractPtps(T)
.

% Mapping control points of the participants involved

cpPtps([]) -> #{}
;
cpPtps([{I, {com, A, B, _}} | T ]) -> maps:put(I, [A, B], cpPtps(T))
;
cpPtps([{I,{cho, A, {Gl,_}, {Gr,_}}} | T ]) ->
    maps:put(I,
	     lists:usort([A] ++ extractPtps(Gl) ++ extractPtps(Gr)),
	     maps:merge(maps:merge(cpPtps(Gl), cpPtps(Gr)), cpPtps(T))
	    )
;
cpPtps([ {I, {bra, _A, Branch}} | T ]) ->
    debug("I = ~p", [I]),
    maps:put(I,
	     lists:usort([_A] ++ lists:flatten(lists:map(fun({G, _}) -> extractPtps(G) end, Branch))),
	     lists:foldr(fun(M1, M2) -> maps:merge(M1, M2) end, cpPtps(T), lists:map(fun({G, _}) -> cpPtps(G) end, Branch))
	    )
;
cpPtps([{I,{par, Gs}} | T ]) ->
    maps:put(I,
	     lists:usort(lists:foldr(fun(Thread,L) -> L ++ extractPtps(Thread) end, [], Gs)),
	     lists:foldr(
	       fun(Thread,Map) -> maps:merge(cpPtps(Thread), Map) end,
	       cpPtps(T),
	       Gs
	      )
	    )
;
cpPtps([{I,{rec, A, {G, _}}} | T ]) -> maps:put(I, lists:usort([A] ++ extractPtps(G)), maps:merge(cpPtps(G), cpPtps(T)))
.
