#!/usr/bin/python3
# Author: Roberto Guanciale <robertog@kth.se>
#
# This script computes the CC2- and CC3-pomset closures

import networkx as nx
from utils import *
from ccpom import *
from projection import proj_to_cfsm
from pomset import get_matching_label

import sys, getopt
from os import listdir
from os.path import isfile, join

def filter_cc3_closure(cc3c):
    filtered_closure = []
    nm = iso.categorical_node_match(["subject", "partner", "in", "out"], ["", "", "", ""])
    for pomset in cc3c:
        found = False
        for pomset1 in cc3c:
            if pomset1 == pomset:
                continue
            nodes1 = [x for x in pomset1.nodes() if frozenset(pomset1.node[x].items()) in
                      [frozenset(pomset.node[y].items()) for y in pomset.nodes()]]
            pomset2 = nx.subgraph(pomset1, nodes1)
            if (nx.is_isomorphic(pomset, pomset2, node_match=nm)):
                found = True
                break
            #prefixes = get_all_prefix_graphs(pomset1, False)
            #for pomset2 in prefixes:
            #    if (nx.is_isomorphic(pomset, pomset2, node_match=nm)):
            #        found = True
            #        break
            if found:
                break
        if not found:
            filtered_closure.append(pomset)
    return filtered_closure

def analysis(inputfolder, outputfolder2, outputfolder3, outputfolder4, draw):
    files = [f for f in listdir(inputfolder) if isfile(join(inputfolder, f)) and f.split(".")[-1] == "graphml"]
    try:
        os.makedirs(outputfolder2)
    except:
        pass
    try:
        os.makedirs(outputfolder3)
    except:
        pass
    try:
        os.makedirs(outputfolder4)
    except:
        pass
    global_view = [nx.readwrite.graphml.read_graphml(join(inputfolder, f)) for f in files]
    cc2c = cc2closure(global_view)
    cc2res = cc2pom(cc2c, global_view)
    cc2err = counterexamples(cc2c, cc2res)
    if draw:
        debug_graphs(cc2err, outputfolder2)
    for i in range(len(cc2err)):
        nx.readwrite.graphml.write_graphml(cc2err[i], join(outputfolder2, "%d.graphml"%i))
    print("Computing C3 closure")
    (cc3c, prefixes) = cc3closure(global_view)
    print("C3 closure computed. Closure %d, Prefixes %d" % (len(cc3c), len(prefixes)))
    cc3c = filter_cc3_closure(cc3c)
    print("C3 filtered %d" %  len(cc3c))
    cc3res = cc3pom(cc3c, prefixes)
    print("C3 res done")
    cc3err = counterexamples(cc3c, cc3res)
    if draw:
        debug_graphs(cc3err, outputfolder3)
    for i in range(len(cc3err)):
        nx.readwrite.graphml.write_graphml(cc3err[i], join(outputfolder3, "%d.graphml"%i))
    projections = []
    corrected_model = global_view
    try:
        os.makedirs(inputfolder + "_corrected")
    except:
        pass

    if len(cc2err) != 0:
        corrected_model = cc2c
    elif len(cc3err) != 0:
        corrected_model = cc3c
        for pomset in corrected_model:
            last_int = max([int(x) for x in pomset.nodes()])
            for node in list(pomset.nodes()):
                if not "out" in pomset.node[node]:
                    continue
                outs = [b for (a, b) in pomset.out_edges(node)]
                found = False
                for node1 in outs:
                    if not "in" in pomset.node[node1]:
                        continue
                    if pomset.node[node1]["subject"] == pomset.node[node]["partner"] and \
                       pomset.node[node]["subject"] == pomset.node[node1]["partner"] and \
                       pomset.node[node1]["in"] == pomset.node[node]["out"]:
                        found = True
                        break
                if not found:
                    last_int += 1
                    pomset.add_node(last_int, **(dict(get_matching_label(pomset.node[node]))))
                    pomset.add_edge(node, last_int)

    for i in range(len(corrected_model)):
        nx.readwrite.graphml.write_graphml(corrected_model[i], join(inputfolder + "_corrected", "%d.graphml"%i))

    
    principals = []
    for pomset in corrected_model:
        for pr in get_all_principals(pomset):
            if not pr in principals:
                principals.append(pr)
    for pr in principals:
        projections.append(proj_to_cfsm(corrected_model, pr))
    if draw:
        debug_graphs(projections, outputfolder4)
    for i in range(len(projections)):
        nx.readwrite.graphml.write_graphml(projections[i], join(outputfolder4, "%d.graphml"%i))
        

def usage():
    print('dirpoms --draw --graphml <input-directory> <ccpom2-out-directory> <ccpom3-out-directory> <proj-folder>')
    
def main(argv):
    inputfolder = ''
    outputfolder2 = ''
    outputfolder3 = ''
    draw = False
    frmt = None
    try:
        opts, args = getopt.getopt(argv,"h",["draw","graphml"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            usage()
            sys.exit()
        elif opt == "--draw":
            draw = True
        elif opt == "--graphml":
            frmt = "graphml"
    if (len(args) != 4):
        usage()
        sys.exit(2)
    inputfolder = args[0]
    outputfolder2 = args[1]
    outputfolder3 = args[2]
    outputfolder4 = args[3]
    analysis(inputfolder, outputfolder2, outputfolder3, outputfolder4, draw)

if __name__ == "__main__":
   main(sys.argv[1:])
