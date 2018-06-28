# Authors: Julien Lange <j.lange@ic.ac.uk> and
#          Emilio Tuosto <emilio@le.ac.uk>
#
# This python script combines the parts of the tool


#!/usr/bin/python
import sys
import subprocess
import os
import os.path
import string
import time
import glob
import argparse

# Tools to combine
cfgfile = 'config.cfg'
with open(cfgfile) as f:
    HKC   = ((f.readline()).split())[-1] + os.sep + "hkc" + os.uname()[0]
    PETRY = ((f.readline()).split())[-1] + os.sep + "petrify" + os.uname()[0]
    GMC   = ((f.readline()).split())[-1]
    BG    = ((f.readline()).split())[-1]

# Setting flags
parser = argparse.ArgumentParser(description="chorgram: From communicating machines to graphical choreographies")
parser.add_argument("-v", "--verbose",
                    dest = "debug",
                    action = "store_true",
                    help = "Run in verbose mode"
)
parser.add_argument("-df",
                    dest = "df",
                    default = "svg",
                    help = "Output format from dot files (svg, png, pdf, etc.) {default = svg}"
)
parser.add_argument("--dot",
                    dest = "dot",
                    action = "append",
                    type=str,
                    help = "Options for dot starting without '-' (e.g., --dot Nnodesep=.5)"
)
parser.add_argument("-l",
                    dest = "leg",
                    action = "store_true",
                    help = "Suppress legend from dot files"
)
parser.add_argument("-dw",
                    dest = "dw",
                    default = "0",
                    help = "Set fixedsize of dot nodes to the given value {default = 0}"
)
parser.add_argument("-ts",
                    dest = "ts",
                    action = "store_true",
                    help = "Just computes the CFSMs and the transition system(s)"
)
parser.add_argument("-tp",
                    dest = "tp",
                    default = "- - - -",
                    help = "Pattern for colouring transitions; the syntax is \"s r d msg\" where s and r are the indexes of sender and receiver, d is the action, and msg is the message {default = \"- - - -\"}"
)
parser.add_argument("-cp",
                    dest = "cp",
                    default = "",
                    help = "Pattern for colouring configurations; the syntax is a string with blank-separated local state ids or '*' (as many as the number of machines) and then some blank separated string of the forom \"s r\" where s and r are the indexes of sender and receiver {default = \"\"}"
)
parser.add_argument("-p", "--path",
                    dest = "path",
                    default = "",
                    help = "Colours paths from the initial node to the ones matching the configuration pattern PATH {default: \"\"}"
)
parser.add_argument("-b", "--bounded",
                    dest = "bound",
                    type = int,
                    default = 0,
                    help = "Set the bound to BOUND; if BOUND < 1, the synchronous TS is computed {default: 0}"
)
parser.add_argument("-nc", "--noclean",
                    dest = "nc",
                    action = "store_false",
                    help = "Do not remove auxiliary files"
)
parser.add_argument("-pn",
                    dest = "pn",
                    help = "Specify the path to petrify  {default: " + PETRY + "}"
)
parser.add_argument("-hkc",
                    dest = "hkc",
                    help = "Specify the path to hkc   {default: " + HKC + "}"
)
parser.add_argument("-gmc",
                    dest = "gmc",
                    help = "Specify the path to gmc   {default: " + GMC + "}"
)
parser.add_argument("-gg",
                    dest = "gg",
                    help = "Specify the path to BuildGlobal   {default: " + BG + "}"
)
parser.add_argument("-dir",
                    dest = "dir",
                    default = "experiments/results",
                    help = "Specify the directory for the output files   {default: outputs}"
)
parser.add_argument("-m",
                    dest = "mul",
                    type = int, default = 0,
                    help = "Specify the multiplicity factor [DEPRECATED]"
)
parser.add_argument("filename",
                    help = "Specify the path to file containing the CFSMs"
)
args = parser.parse_args()


debug = True if args.debug else False
if args.hkc: HKC   = args.hkc
if args.pn:  PETRY = args.pn
if args.gmc: GMC   = args.gmc
if args.gg:  BG    = args.gg

bname = os.path.basename((os.path.splitext(args.filename))[0])
dir = args.dir + ("" if (args.dir)[-1] == os.sep else os.sep) + bname + os.sep
try:
    os.makedirs(dir)
except OSError:
    if os.path.exists(dir):
        pass
    else:
        raise
basename = dir + bname      # os.path.basename(args.filename)

factor = str(args.mul)

PROJ = "_projection_"
MACH = "_machine_"
TEMP = "tempefc"
PNET = "_petrinet"
FINP = "_finalpn"
PGLO = "_preglobal"
GLOB = "_global"


############################### START HERE ###################################

def debugMsg(msg, force = False):
    """Prints debugging messages"""
    if args.debug or force:
        print "chorgram: " + ("{---" if not(force) else "") + msg + ("---}" if not(force) else "")

debugMsg("\n   Executing with...\n\tgmc\t\t" + GMC +
         "\n\tBuildGraph\t" + BG +
         "\n\tHKC\t\t" + HKC +
         "\n\tPETRIFY\t\t" + PETRY +
         "\n\tmultiplicity\t" + factor +
         "\n\tdir\t\t" + args.dir +
         "\n\tCFSMs\t\t" + args.filename + "\n"
)

date = time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
starttime = time.time()

callgmc = ([GMC,
            "-d", args.dir,
            "-p", args.path,
            "-m", str(args.mul),
            "-tp", args.tp,
            "-cp", args.cp,
            "-b", str(args.bound)] +
           (["-l"] if args.leg else []) +
           (["-ts"] if args.ts else []) +
           [args.filename]
)

debugMsg("Execution Started on " + date, True)
debugMsg(string.join(callgmc))
gmctime = time.time()
subprocess.check_call(callgmc)

with open(dir + '.machines') as f:
    machines = (f.readline()).split()
    machine_number = len(machines)

if machine_number < 2:
    debugMsg("Less than 2 machines! Bye...",True)
    sys.exit()

if args.ts:
    sys.exit()
else:
    debugMsg("Checking projections...")

tmpbool = True

hkctime = 0

for i in range(machine_number):
    debugMsg("Testing machine "+ MACH + str(i) + " for language equivalence")
    debugMsg(HKC + " -equiv " + basename + MACH + str(i) + " " + basename + PROJ + str(i))
    start = time.time()
    try:
        cmd = subprocess.Popen([HKC,
                                "-equiv",
                                basename + MACH + str(i),
                                basename + PROJ + str(i)],
                                stdout=subprocess.PIPE
        )
    except: debugMsg("Language equivalence check failed. Something wrong with " + HKC)
    stop = time.time()
    hkctime = hkctime + stop - start
    for line in cmd.stdout: spa = line    # read the last line produced by hkc
    spa = line.split(': ')                # after ': ' on the last line of its output hkc returns the boolean
    hkc_boolean_position = 1              # position of the boolean returned by hkc after the split
    res = spa[hkc_boolean_position].split(',')[0]
    tmpbool = tmpbool and (res == "true")
    debugMsg("Is machine " + str(i) + " representable? "+ res, True)

debugMsg("Language-equivalence (Representability part (i))? " + str(tmpbool), True)


######################################## PETRIFY ###############################################

debugMsg("Calling petrify...")
start = time.time()
try:
    subprocess.check_call([PETRY,
                           "-dead","-ip",
                           "-efc", basename + "_toPetrify",
                           "-o" , dir + TEMP]
    )
except: debugMsg("Petrification failed. Something wrong with " + PETRY)
stop = time.time()
petritime = stop - start
st_arrow, st_comma, st_colon,st_del = "AAA", "CCC", "COCO", "delPTP"
replacements = [(st_comma,', '), # NOTE: the order is important
                (st_arrow,'->'),
                (st_colon,':'),
                (st_del,""),] +\
                [(" " + str(i), machines[i]) for i in range(machine_number)] +\
                [("->" + str(i), "->" + machines[i]) for i in range(machine_number)] +\
                [(', ', ',')]
try:
    with open(dir + TEMP, "rt") as fin:
        lines =  fin.readlines()
        fin.close()
        for (src,target) in replacements:
            lines = map(lambda x: x.replace(src,target), lines)
        with open(basename + PNET, "wt") as fout:
            for l in lines: fout.write(l)
        debugMsg("Generating Global Graph...")
        ggstarttime = time.time()
        subprocess.check_call([BG, "-d" , dir, basename + PNET])
        endtime = time.time()
except: debugMsg("Something wrong with the generation of global graph...")
debugMsg("All done.\n\tTotal execution time: " +  str(endtime - starttime) +
         "\n\t\tGMC check:\t\t\t" + str( gmctime - starttime) +
         "\n\t\tHKC minimisation:\t\t" + str(hkctime) +
         "\n\t\tPetrify:\t\t\t" + str(petritime) +
         "\n\t\tGlobal graph generation:\t" + str(endtime - ggstarttime),
         True
)

########################################## DOT #################################################
debugMsg("Transforming dot files in " + args.df + " format", True)
dot = ["dot"] + ([] if (args.dot==None) else (['-' + d for d in args.dot])) + ["-T" , args.df]
for x in ["machines", "ts0"] + (["ts" + str(args.bound)] if args.bound > 0 else []) + (["projection_"+ str(i) for i in range(machine_number)]) :
    debugMsg("Dot-tifying " + basename + "_" + x + ".dot")
    subprocess.call(dot + [basename + "_" + x + ".dot"] + ["-o", basename + "_" + x + "." + args.df])
if debug:
    subprocess.check_call(dot + [basename + PNET + FINP + ".dot", "-o", basename + FINP + "." + args.df])
    subprocess.check_call(dot + [basename + PNET + PGLO + ".dot", "-o", basename + PGLO + "." + args.df],
                          stderr=subprocess.PIPE
    )
subprocess.check_call(dot + ["-Gsplines=ortho", basename + PNET + GLOB + ".dot", "-o", basename + GLOB + "." + args.df],
                      stderr=subprocess.PIPE
)

###################################### CLEANING UP #############################################

if args.nc and not debug:
    to_be_deleted = [
        f for sub in (glob.glob("./" + basename + x + "*")
                      for x in [PROJ,MACH,PNET,"_toPetrify", FINP, "_preglobal*"]
        )
        for f in sub
        ] + [dir + ".machines", dir + TEMP]
    debugMsg("Deleting auxiliary files")
    for fl in to_be_deleted:
        debugMsg("\tDeleting " + fl)
        os.remove(fl)
