#!/usr/bin/env python

# Authors: Emilio Tuosto
##

import sys
import os
import os.path
import string
import subprocess

# Auxiliary functions
def execute(cmd, err):
    """Execute a shell command"""
    try: subprocess.call([cmd], shell=True)
    except: print(err)

def mk_erl(template, name, gg = ""):
    """Create the Erlang file for gg"""
    values = {}
    values['name'] = name
    with open(template) as f:
        tmp = f.read()
    with open(gg) as f:
        erlGG = f.read()
    values['gg'] = erlGG
    fmt = tmp % values
    with open(name + ".erl", "w") as f:
        f.write(fmt)

if __name__ == "__main__":
    """generates the code for the demo and runs it forever"""
    execute("erlc aux.erl rgg_support.erl gg2erl.erl", "??? compiler")
    template = sys.argv[1]
    name = sys.argv[2]
    erlgg = sys.argv[3]
    #
    # concatenate CFSMs to form a communicating system in the file sys.fsa 
    demodir = os.path.dirname(os.path.splitext(erlgg)[0]) + os.sep
    execute("rm -f " + demodir + "sys.fsa", "bad cat")
    execute("cat " + demodir + "*.fsa > " + demodir + "sys.fsa", "bad cat")
    #
    # png of the global graph
    execute("dot -T png -Gsplines=ortho -Gnodesep=2 " + demodir + "graph_sgg.dot -o " + demodir + "graph_sgg.png", "dot")
    #
    # compile the compiles
    mk_erl(template, name, erlgg)
    execute("erlc " + name + ".erl","")
    #
    # do generate a trace, compile it, and run it...forever
    trace = "erl -eval \'io:format(" + name + ":main())\' -s init stop"
    comp  = "erlc " + name + "_demo.erl"
    run   = "erl -eval \'io:format(" + name + "_demo:main())\' -s init stop"
    while True:
        execute(trace, "??? trace")
        execute(comp, "??? comp")
        execute(run, "??? run")
