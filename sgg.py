#!/usr/bin/python


# Author: Emilio Tuosto <emilio@le.ac.uk>
#
# This python script combines several parts of ChorGram

import sys
import subprocess
import os
import os.path
import string
import time
import glob
import argparse

from utils import *

SGG = "sgg"

# Setting flags
parser = argparse.ArgumentParser(description="sgg: semantics of syntactic global graphs and their projections to communicating machines")
parser.add_argument("-v", "--verbose",
                    dest = "debug",
                    action = "store_true",
                    help = "Run in verbose mode")
parser.add_argument("-df",
                    dest = "df",
                    default = "svg",
                    help = "Output format from dot files (svg, png, pdf, etc.) {default = svg}")
parser.add_argument("--dot",
                    dest = "dot",
                    action = "append",
                    type=str,
                    help = "Options for dot starting without '-' (e.g., --dot Nnodesep=.5)")
parser.add_argument("-l",
                    dest = "leg",
                    action = "store_true",
                    help = "Suppress legend from dot files")
parser.add_argument("--dir",
                    dest = "dir",
                    default = "experiments/results",
                    help = "Specify the directory for the output files   {default: outputs}")
parser.add_argument("--sloppy",
                    dest = "sloppy",
                    action = "store_true",
                    help = "Do not raise exception due to non well-formedness")
parser.add_argument("-rg",
                    dest = "rg",
                    action = "store_true",
                    help = "Uses the parser for REGs")
parser.add_argument("filename",
                    help = "Specify the path to file containing the CFSMs")
args = parser.parse_args()

bname = os.path.basename((os.path.splitext(args.filename))[0])
dir = args.dir + ("" if (args.dir)[-1] == os.sep else os.sep) + bname + os.sep
mkdir(dir)


##################################### START HERE ###############################################

debugMsg(args.debug, SGG, "\n   Generating " + bname + "\n\tResult in " + dir + "\n")

template = """
-module(%s).
 
 -export([main/0]).
 
 -import(aux, [str_cat/2]).
 
getTest(%s) -> %s
.

gTrace([])-> maps:new()
;

gTrace([{_,{com,_,_,_}} | T ]) -> gTrace(T)
;

%% the rand for branches is on 2
gTrace([{I, {cho, _, {Gl,_}, {Gr,_}}} | T ]) ->
    maps:put(integer_to_list(I), integer_to_list(rand:uniform(2)),
	     maps:merge(maps:merge(gTrace(Gr), gTrace(Gl)), gTrace(T))
	    )
;

%% the rand for branches is on 2
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
    %%  [{I, get(itr)}] ++ gTrace(G) ++ gTrace(T)
.
 
set_map(Test, Itr, Debug, Reporting, Revmod, Loopmod) ->
    put(itr, (if Itr < 0 -> rand:uniform(10) - 1; Itr >= 0 -> Itr end)),
    maps:put(debug, Debug,
	     maps:put(revMod, Revmod,
		      maps:put(loopMod, Loopmod,
			       maps:put(reporting, Reporting,
					maps:put(rnd, gTrace(getTest(Test)), maps:new()))
			      )
		     )
	    )
.

main() ->
    %% gg2erl:mk_all compiles getTest(%s) into an Erlang file
    %% named "%s_%s.erl" setting loops' iterations (3
    %% below), debug level (4 below), setting the flag about reporting
    %% messages (true below), and using the standard semantics of GGs
    %% (std below)
    M = set_map(%s, 3, 10, true, std, std),
    gg2erl:mk_all("%s_%s", getTest(%s), M)
.
"""
module_name = 'xyz'
test_ds = '[...]'
app_name = 'app'

# print(template%(module_name, module_name, test_ds, module_name, module_name, app_name, module_name, module_name, app_name, module_name))

starttime = time.time()

callsgg = ([SGG, "-d", args.dir] +
           (["-l"] if args.leg else []) +
           (["--sloppy"] if args.sloppy else []) +
           (["-rg"] if args.rg else []) +
           [args.filename])

debugMsg(args.debug, SGG, ' '.join(callsgg))
sggtime = time.time()
subprocess.check_call(callsgg)

