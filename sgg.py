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

starttime = time.time()

callsgg = ([SGG, "-d", args.dir] +
           (["-l"] if args.leg else []) +
           #
           # (["--sloppy"] if args.sloppy else []) +
           # for the moment --sloppy has to be the default
           #
           (["--sloppy"]) +
           (["-rg"] if args.rg else []) +
           [args.filename])

debugMsg(args.debug, SGG, ' '.join(callsgg))
sggtime = time.time()
subprocess.check_call(callsgg)

