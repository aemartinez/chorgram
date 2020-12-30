#!/usr/bin/python3
#
# Author: Emilio Tuosto <emilio@le.ac.uk>
#
# This script converts a system of CFSMs in graphml format to the
# 'fsa' format of chorgram
#

import getopt, sys
import networkx as nx
from os import listdir, curdir
from os.path import isfile, join, splitext, basename

def mkQ(x):
    '''State names cannot start with numbers to be digested by petrify'''
    return "q" + x

def get_init(gr):
    '''Return initial state and subject of an automaton in graphml-format'''  
    # PRE: all edges in gr have the same subject
    # POST: return the pair made of the initial state and the subject of the machine
    #
    for q0 in gr.nodes():
        if 'initial' in gr.nodes[q0].keys():
            break
    edge0 = list(gr.edges())[0]
    sbj = gr.edges[edge0]['subject']
    return (mkQ(q0),sbj)


def to_fsa_action(e, ptps):
    '''Transforms edges in graphml format in fsa format'''
    # PRE: e is an edge of a graphml object representing a CFSM
    # POST: return 'fsa' string of the action
    #
    res = ' ! ' + e['out'] if 'out' in e.keys() else ' ? ' + e['in']
    return str(ptps[e['partner']]) + res

def graphml_to_fsa(gr,ptps,init_sbj):
    '''Compute the fsa-version of a graphml automaton'''
    # PRE: gr represents a CFSM and ptps  maps participants' names to integers (to convert names into indexes of CFSMs)
    # return the fsa format of the graphml gr using
    #
    fsa_start = ".outputs %s\n.state graph"
    fsa_stop = ".marking %s\n.end\n"
    # convert each edge into a transition of the fsa
    (q0,sbj) = (init_sbj[0],init_sbj[1])
    fsa_trx = "\n".join([" ".join([mkQ(s),
                                   to_fsa_action(gr.edges[s, t],ptps),
                                   mkQ(t)])
                         for (s,t) in gr.edges()]
    )
    return "\n".join([fsa_start % sbj, fsa_trx, fsa_stop % q0])

def graphsml_to_fsa(gmldir):
    '''Transforms all the graphml automata of a directory in the fsa-format.'''
    # PRE: gmldir is the path to a directory containing
    #      the CFSMs in graphml files; each file is a CFSM
    # POST: the 'fsa' format of the system of CFSMs
    #
    gmlfiles = [f for f in listdir(gmldir) if isfile(join(gmldir, f)) and splitext(basename(f))[1] == ".graphml"]
    gmls = [nx.readwrite.graphml.read_graphml(join(gmldir, f)) for f in gmlfiles]
    init_sbj = [get_init(gr) for gr in gmls]
    ptps = dict(zip([x[1] for x in init_sbj], range(len(init_sbj))))
    return "\n\n".join(map(lambda z : graphml_to_fsa(z[0],ptps,z[1]), zip(gmls,init_sbj)))

def usage():
    print('gml2fsa [-h] [-o <output-file>] <input-directory>')
    
def main(argv):
    try:
        opts, args = getopt.getopt(argv,"o: h")
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            usage()
            sys.exit()
    if (len(args) > 3):
        usage()
        sys.exit(2)
    res = graphsml_to_fsa(args[0])
    if opt != "-o":
        print(res)
    else:
        f = open(opts[0][1], "w")
        f.write(res)
        f.close()

if __name__ == "__main__":
    main(sys.argv[1:])
