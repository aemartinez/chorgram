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
                    default = "./",
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
f, ext = os.path.splitext(sgg)                 # if sgg = "a/b/c.ext" then f = "a/b/c", ext = ".ext",
bname = os.path.basename(f)                    # and bname = "a/b/c"
mkdir(os.path.expanduser(dir) + bname)         # dir + bname is where resulting files are stored

# get the erlang data structure of the global graph
#
saySomething("Processing " + bname + ext + " :: result in " + dir + "...")
callsgg = ([SGG, "-d", dir] +
           (["--sloppy"] if args.sloppy else []) +
           (["-rg"]) +
           [args.dir + sgg])
subprocess.check_call(callsgg)

# prepare the erlang file from the template
#
with open("erlang.template") as f:
    codeTemplate = string.join(f.readlines())
with open(dir + bname + "/reg.txt") as f:
    erlGG = f.readlines()
with open(dir + bname + ".erl", "w") as f:
    f.write(codeTemplate % (string.join(erlGG)))
saySomething("Erlang file to compile the demo in " + dir + bname + ".erl")
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
