# Author: Emilio Tuosto <emilio@le.ac.uk>
#
# Script for CO7205

#!/usr/bin/python

import sys
import subprocess
import os
import ntpath
import string
import glob
import argparse

from utils import *

SGG, dotCFG, cmdPref, test = "./sgg", ".dot.cfg", "demo", "test"

# Setting flags
parser = argparse.ArgumentParser(description="demo: script for running gg2erl")
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
                    default = "/tmp/",
                    help = "Specify the directory for the output files   {default: current directory}")
parser.add_argument("--sloppy",
                    dest = "sloppy",
                    action = "store_true",
                    help = "Do not raise exception due to non well-formedness   {default: true}")
parser.add_argument("filename",
                    help = "Specify the path to file containing the CFSMs")
args = parser.parse_args()

def saySomething(msg):
    """Prints messages"""
    print "|gg2erl| : \t" + msg

_ = os.system('clear')


##################################### START HERE ###############################################

saySomething("Let's start!")

# set file names and directories
#
dir = args.dir + ("" if (args.dir)[-1] == os.sep else os.sep)
sgg = args.filename
name, ext = os.path.splitext(sgg)              # if sgg = "a/b/c.ext" then f = "a/b/c", ext = ".ext",
basename = os.path.basename(name)                 # and basename = "a/b/c"
mkdir(os.path.expanduser(dir) + basename)         # dir + basename is where resulting files are stored

# get the erlang data structure of the global graph
#
saySomething("Processing " + basename + ext + " :: result in " + dir + basename + "...")
callsgg = ([SGG, "-d", dir] +
           (["--sloppy"] if args.sloppy else []) +
           ["-rg", sgg])
subprocess.check_call(callsgg)

# prepare the erlang file from the template
#
codeTemplate = """
-module(%s).

-export([ main/0 ]).
-import(aux, [ set_map/6 ]).

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

getGG() -> %s.

main() ->
    gg2erl:mk_all("%s", getGG(), set_map(%s, rand:uniform(10) - 1, 1, false, std, std)).
"""

with open(dir + basename + os.sep + "reg.txt") as f:
    erlGG = f.readlines()
with open("../../Dropbox/mypapers/reversibleActors/code/" + basename + ".erl", "w") as f:
    f.write(codeTemplate % (basename, string.join(erlGG), basename, basename))
saySomething("Erlang file to compile " + basename + ".erl")
saySomething("Have fun now :)")


########################################## DOT #################################################
# dotmap = {}
# with open(dotCFG) as f:
#     lines = f.readlines()
# for i in range( len(lines) -1 ) :
#     pair = (lines[i+1]).split()
#     key  = pair[0]
#     val  = pair[1]
#     dotmap[key] = val

# dot = ["dot"] + ([] if (args.dot==None) else (['-' + d for d in args.dot]))
# copt = ["-T"          + (dotmap['ggfmt'] if (args.df==None) else args.df),
#         "-Gnodesep="  + dotmap['ggnodesep'],
#         "-Nfontname=" + dotmap['nodefont'],
#         "-Efontname=" + dotmap['edgefont']]
# gopt = copt + ["-Gsplines=" + dotmap['gglines']]
# sopt = copt + ["-Gsplines=" + dotmap['semlines']]

# for x in [d for d in os.listdir(dir) if d[-4:] == ".dot" and d[:4] != "cfsm" and d[:3] != "sem"]:
#     debugMsg(args.debug, cmdPref, "Dot-tifying " + dir + x)
#     subprocess.call(dot + gopt + [dir + x] + ["-o", dir + x[:-3] + args.df])

# for x in [d for d in os.listdir(dir) if d[-4:] == ".dot" and d[:4] == "cfsm"]:
#     debugMsg(args.debug, cmdPref, "Dot-tifying " + dir + x)
#     subprocess.call(dot + [gopt[0]] + [dir + x] + ["-o", dir + x[:-3] + args.df])

# debugMsg(args.debug, cmdPref, "Dot-tifying " + dir + "sem_sgg.dot")
# subprocess.call(dot + sopt + [dir + "sem_sgg.dot"] + ["-o", dir + "sem_sgg." + args.df])
