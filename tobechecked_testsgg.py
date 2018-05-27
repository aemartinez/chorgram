# Author: Emilio Tuosto <emilio@le.ac.uk>
#
# This python script combines the parts of the tool

#!/usr/bin/python
import sys
import subprocess
import string
import time
import glob
import argparse

from utils import *

SGG = "./sgg"
dotCFG = '.dot.cfg'
cmdname = "sgg"

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
parser.add_argument("-dir",
                    dest = "dir",
                    default = "experiments/results",
                    help = "Specify the directory for the output files   {default: outputs}")
parser.add_argument("filename",
                    help = "Specify the path to file containing the CFSMs")
args = parser.parse_args()

basename_ = os.path.basename((os.path.splitext(args.filename))[0])
dir = args.dir + ("" if (args.dir)[-1] == os.sep else os.sep) + basename_ + os.sep
basename = dir + basename_ # os.path.basename(args.filename)
mkdir(dir)

##################################### START HERE ###############################################

debugMsg(args.debug, cmdname, "\n   Generating " + basename_ + "\n\tResult in " + dir + "\n")

starttime = time.time()

callsgg = ([SGG, "-d", args.dir] +
           (["-l"] if args.leg else []) +
           [args.filename])

debugMsg(args.debug, cmdname, string.join(callsgg))
sggtime = time.time()
subprocess.check_call(callsgg)

########################################## DOT #################################################
dotmap = {}
with open(dotCFG) as f:
    lines = f.readlines()
for i in range( len(lines) -1 ) :
    pair = (lines[i+1]).split()
    key  = pair[0]
    val  = pair[1]
    dotmap[key] = val

dot = ["dot"] + ([] if (args.dot==None) else (['-' + d for d in args.dot]))
copt = ["-T"          + (dotmap['ggfmt'] if (args.df==None) else args.df),
        "-Gnodesep="  + dotmap['ggnodesep'],
        "-Nfontname=" + dotmap['nodefont'],
        "-Efontname=" + dotmap['edgefont']]
gopt = copt + ["-Gsplines=" + dotmap['gglines']]
sopt = copt + ["-Gsplines=" + dotmap['semlines']]

for x in [d for d in os.listdir(dir) if d[-4:] == ".dot" and d[:4] != "cfsm" and d[:3] != "sem"]:
    debugMsg(args.debug, cmdname, "Dot-tifying " + dir + x)
    subprocess.call(dot + gopt + [dir + x] + ["-o", dir + x[:-3] + args.df])

for x in [d for d in os.listdir(dir) if d[-4:] == ".dot" and d[:4] == "cfsm"]:
    debugMsg(args.debug, cmdname, "Dot-tifying " + dir + x)
    subprocess.call(dot + [gopt[0]] + [dir + x] + ["-o", dir + x[:-3] + args.df])

debugMsg(args.debg, cmdname, "Dot-tifying " + dir + "sem_sgg.dot")
subprocess.call(dot + sopt + [dir + "sem_sgg.dot"] + ["-o", dir + "sem_sgg." + args.df])
