# Author: Emilio Tuosto <emilio@le.ac.uk>
#
# Script for the demo at ICE2018

#!/usr/bin/python

import sys
import subprocess
import os
import os.path
import string
import glob
import argparse

from utils import *

SGG, dotCFG, cmdPref, test = "./sgg", ".dot.cfg", "demo", "test"

# Setting flags
parser = argparse.ArgumentParser(description="demo: script for demoing gg to erlang")
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
                    default = "../../emtalks/behAPI_ice18/demo/",
                    help = "Specify the directory for the output files   {default: ~/emtalks/behAPI_ice18/demo/}")
parser.add_argument("--sloppy",
                    dest = "sloppy",
                    action = "store_true",
                    help = "Do not raise exception due to non well-formedness")
parser.add_argument("-rg",
                    dest = "rg",
                    action = "store_true",
                    help = "Uses the parser for REGs")
args = parser.parse_args()

##################################### START HERE ###############################################

# set files
sgg = "atm.sgg"
bname = os.path.basename((os.path.splitext(sgg))[0])
dir = args.dir + ("" if (args.dir)[-1] == os.sep else os.sep) # + bname + os.sep
print(os.path.expanduser(dir) + bname)
mkdir(os.path.expanduser(dir) + bname)

# get the erlang data structure of the global graph
debugMsg(args.debug, cmdPref, "\n   Generating " + bname + "\n\tResult in " + dir + "\n")
callsgg = ([SGG, "-d", dir] +
           (["--sloppy"] if args.sloppy else []) +
           (["-rg"]) +
           [args.dir + sgg])
debugMsg(args.debug, cmdPref, string.join(callsgg))
subprocess.check_call(callsgg)

# prepare the erlang file from the template
with open(dir + "_" + test + ".erl") as f:
    codeTemplate = string.join(f.readlines())
with open(dir + bname + "/rgg.txt") as f:
    erlGG = f.readlines()
code = codeTemplate % (string.join(erlGG))
with open("../../Dropbox/mypapers/reversibleActors/code/" + test + "_ice18" + ".erl", "w") as ice:
    ice.write(code)


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
