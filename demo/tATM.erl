-module(tATM).
 
 -export([main/0]).
 
 -import(aux, [str_cat/2]).

getName() -> "tATM"
.

getTest() -> [ { 20, { com, ptp_C, ptp_A, auth } }, { 19, { com, ptp_A, ptp_B, authReq } }, { 18, { bra, ptp_B, [ { [ { 17, { com, ptp_B, ptp_A, denied } }, { 16, { com, ptp_A, ptp_C, authFail } } ], " " } ] ++ [ { [ { 15, { com, ptp_B, ptp_A, granted } }, { 14, { bra, ptp_C, [ { [ { 13, { com, ptp_C, ptp_A, withdraw } }, { 12, { com, ptp_A, ptp_B, authWithdrawal } }, { 11, { bra, ptp_B, [ { [ { 10, { com, ptp_B, ptp_A, allow } }, { 9, { com, ptp_A, ptp_C, money } } ], " " } ] ++ [ { [ { 8, { com, ptp_B, ptp_A, deny } }, { 7, { com, ptp_A, ptp_C, bye } } ], " " } ] } } ], " " } ] ++ [ { [ { 6, { com, ptp_C, ptp_A, checkBalance } }, { 5, { com, ptp_A, ptp_B, getBalance } }, { 4, { com, ptp_B, ptp_A, balance } }, { 3, { com, ptp_A, ptp_C, balance } } ], " " } ] ++ [ { [ { 2, { com, ptp_C, ptp_A, quit } }, { 1, { com, ptp_A, ptp_B, quit } } ], " " } ] } } ], " " } ] } } ]
.

gTrace([])-> maps:new()
;

gTrace([{_,{com,_,_,_}} | T ]) -> gTrace(T)
;

% the rand for branches is on 2
gTrace([{I, {cho, _, {Gl,_}, {Gr,_}}} | T ]) ->
    maps:put(integer_to_list(I), integer_to_list(rand:uniform(2)),
	     maps:merge(maps:merge(gTrace(Gr), gTrace(Gl)), gTrace(T))
	    )
;

% the rand for branches is on 2
gTrace([{I, {bra, _, Branch}} | T ]) ->
    maps:put(integer_to_list(I), integer_to_list(rand:uniform(length(Branch))),
	     lists:foldr(fun({M1,_}, M2) -> maps:merge(gTrace(M1),M2) end, gTrace(T), Branch )
	    )
;

gTrace([{I, {par, Gs}} | T ]) ->
    case Gs of
	[]        -> gTrace(T);
	[G | Gs1] -> maps:merge(gTrace(G), gTrace([{I,{par, Gs1}} | T]))
    end
;

gTrace([{I, {rec, _, {G, _}}} | T ]) ->
    maps:put(integer_to_list(I), integer_to_list(get(itr)), maps:merge(gTrace(G), gTrace(T)))
    %  [{I, get(itr)}] ++ gTrace(G) ++ gTrace(T)
.
 
set_map(Itr, Debug, Reporting, Revmod, Loopmod) ->
    put(itr, (if Itr < 0 -> rand:uniform(10) - 1; Itr >= 0 -> Itr end)),
    maps:put(debug, Debug,
	     maps:put(revMod, Revmod,
		      maps:put(loopMod, Loopmod,
			       maps:put(reporting, Reporting,
					maps:put(rnd, gTrace(getTest()), maps:new()))
			      )
		     )
	    )
.

main() ->
    % gg2erl:mk_all compiles getTest() into an Erlang file
    % named "riocuarto19_atm.erl" setting loops' iterations (3
    % below), debug level (4 below), setting the flag about reporting
    % messages (true below), and using the standard semantics of GGs
    % (std below)
    M = set_map(3, 10, true, std, std),
    gg2erl:mk_all(getName() ++ "_demo", getTest(), M)
.
