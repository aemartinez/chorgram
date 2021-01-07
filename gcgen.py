#!/usr/bin/python3


# Author: Emilio Tuosto <emilio.tuosto@gssi.it>
#
# This python script generates random well-formed g-choreographies.
# For a size, say k, the script returns a g-choreographies which
# combines at lest k gates (sequential, parallel, choice, and
# iteration). Branches of a choice can be complex g-choreographies
# bound by the number of participants of the choice.
#

import random
import sys
import os
import os.path
import string
import glob
import argparse

from datetime import datetime

from utils import *

# Get inputs
parser = argparse.ArgumentParser(description="gcgen: generates random well-formed g-choreographies")
parser.add_argument("-v", "--verbose",
                    dest = "debug",
                    action = "store_true",
                    help = "Run in verbose mode")
parser.add_argument("-o", "--output",
                    dest = "output",
                    default = "",
                    help = "store results in files {default = \"\"}")
parser.add_argument("-s", "--size",
                    dest = "size",
                    default = -1,
                    help = "bound to the size of the generated g-choreographies; -1 stops randomly {default = -1}")
parser.add_argument("-p", "--participants",
                    dest = "ptps",
                    default = -1,
                    help = "maximal number of participants in the generated g-choreographies; -1 random {default = -1}")
parser.add_argument("-m", "--messages",
                    dest = "msgs",
                    default = -1,
                    help = "maximal number of messages in the generated g-choreographies; -1 random {default = -1}")
parser.add_argument("-n", "--gc",
                    dest = "gc",
                    default = 1,
                    help = "number of g-choreographies to generate; use '-1' for generating infititely many g-choreographies {default = 1}")
args = parser.parse_args()

size = int(args.size) if int(args.size) > -1 else random.randint(1,RND_SIZE)
ptps = int(args.ptps) if int(args.ptps) > -1 else random.randint(2,RND_PTPS)
msgs = int(args.msgs) if int(args.msgs) > -1 else random.randint(1,RND_MSGS)
ngcs = int(args.gc)
outs = args.output

cmd = ".. "
debugMsg(args.debug,
         cmd,
         "Size: {}\tMax participants: {}\tMax messages: {}".format(str(size), str(ptps), str(msgs))
)

def decSize():
    global size
    assert size >= 0, "Negative size"
    size = size - 1

emp = "(o)"

gates = ["seq", "par", "sel", "emp", "rep"]

indent = "  "
level = 0

def mkIndent(s):
    return "\n" + (level*indent) + s

def mkPtp(p):
    return "P" + str(p)

def mkMsg(m, lab = "m"):
    return lab + str(m)

def mkBlock(b):
    tab = level*indent
    return ("\n{}{{{}\n{}}}".format(tab, b, tab))

def mkArrow(s, r, m):
    return mkIndent("{} -> {}: {}".format(mkPtp(s), mkPtp(r), m))

def getInteraction(sender = None, receiver = None, m = None):
    assert (sender == None and receiver == None) or (sender != None and receiver != None), "Either both sender and receiver or none"
    if sender == None:
        ps = [0,0]
        while ps[0] == ps[1]:
            ps = random.sample(range(1, ptps+1), 2)
        sender = ps[0]
        receiver = ps[1]
    msg = m if m else random.randint(1,RND_MSGS)
    return (mkArrow(sender, receiver, mkMsg(msg)))

def getSeq():
    return (getGC() + " ;" + getGC())

def getPar():
    return (getGC() + mkIndent("|") + getGC())

def mkChoice(a, l, m):
    return ";".join([mkArrow(a, l[i], m) for i in range(len(l))])

def getBra(a = None, p = None, m = None, branching = 5):
    ### TODO: make the maximal number of branches variable
    pts = random.sample(range(1,ptps+1), random.randint(2,ptps))
    active = a if a else pts[0]
    passive = p if p else pts[1:]
    b = random.randint(2, branching)
    branches = [(random.sample(passive, len(passive)) , (m if m else "") + str(i)) for i in range(1,b+1)]
#    return (active, (mkIndent("+")).join(map((lambda x: mkChoice(active, x[0], mkMsg(x[1], "b"))), branches)))
    return (active, (mkIndent("+")).join(map((lambda x: mkGC([active], x[0], mkMsg(x[1], "b"))), branches)))

def getLoop():
    pts = random.sample(range(1,ptps+1), random.randint(2,ptps))
    return (pts[0], mkGC([pts[0]], pts[1:], mkMsg(random.randint(1,RND_MSGS))))
    
def getGC():
    t = datetime.now()
    random.seed(t)
    global level
    if size == 0:
        res = getInteraction()  # mkIndent(emp)
    else:
        gate = random.choice(gates)
        if gate == "emp":
            level = level + 1
            boby = getGC()
            level = level - 1
            res = mkBlock(boby)
        elif gate == "int":
            res = getInteraction()
        elif gate == "seq":
            decSize()
            res = getGC() + ";" + getGC()
        elif gate == "par":
            decSize()
            res = getGC() + mkIndent("|") + getGC()
        elif gate == "sel":
            decSize()
            level = level + 1
            (active, body) = getBra()
            level = level - 1
            res = (mkIndent("sel " + mkPtp(active) + " {") + body + mkIndent("}"))
        elif gate == "rep":
            decSize()
            level = level + 1
            (p, body) = getLoop()
            level = level - 1
            res = (mkIndent("repeat " + mkPtp(p) + " {") + body + mkIndent("}"))
        else:
            raise ("unknown gate: " + gate)
    return res

def mkGC(aware, ptps, brc):
    assert ptps == [] or aware != [], "No one's aware!"
    global level
    if ptps == []:
        return emp
    elif len(ptps) == 1:
        res = getInteraction(random.choice(aware), ptps[0], brc)
    else:
        gate = random.choice(gates)
        l = len(ptps)//2
        if gate == "emp":
            res = mkGC(aware, ptps, brc)
        elif gate == "int":
            mkGC(aware, ptps, brc)
        elif gate == "seq":
            res = mkGC(aware, ptps[:l], brc) + ";" + mkGC(aware + ptps[:l], ptps[l:], brc)
        elif gate == "par":
            level = level + 1
            res = mkGC(aware, ptps[:l], brc) + mkIndent("|") + mkGC(aware, ptps[l:], brc)
            level = level - 1
            res = mkIndent("{") + res + mkIndent("}")
        elif gate == "sel":
            active = random.choice(aware)
            level = level + 1
            (active, body) = getBra(a = active, p = ptps[:l], m = brc)
            level = level - 1
            cntd = mkGC(aware + ptps[:l], ptps[l:], brc)
            res = (mkIndent("sel " + mkPtp(active) + " {") + body + mkIndent("}"))
            res = res + ";" + mkGC(aware + ptps[:l], ptps[l:], brc)
        elif gate == "rep":
            level = level + 1
            body = mkGC(aware, ptps, brc)
            level = level - 1
            res = (mkIndent("repeat " + mkPtp(ptps[0]) + " {") + body + mkIndent("}"))
        else:
            raise ("unknown gate: " + gate)
    return res
        
def main():
    """gcgen: generates random well-formed g-choreographies"""
    flag = outs!="" and (ngcs <0)
    debugMsg(args.debug, "gcgen: ", "Carefull!!! storing infinitely many results!", flag)
    confirm = ""
    if flag:
        while confirm != "YES" and confirm != "NO":
            confirm = raw_input("Please confirm that you really want to do it. Should I continue? [YES | NO]: ")
    if not flag or confirm == "YES":
        i = 1
        while i <= ngcs or ngcs == -1:
            global size
            size = int(args.size) if int(args.size) > -1 else random.randint(1,RND_SIZE)
            gc = getGC()
            if outs == "":
                print(".. #{}{}\n".format(i, gc))
            else:
                filename = "{}_{}.gc".format(outs, i)
                with open(filename, "w") as f:
                    f.write(".. Randomly generated by gcgen\n{}".format(gc))
                    print("gcgen: {} saved".format(filename))
            i = i + 1
    else:
        sys.exit("Good bye")

if __name__ == "__main__":
    main()

