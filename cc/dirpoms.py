#!/usr/bin/python3
import networkx as nx
from utils import *
from ccpom import *
from projection import proj_to_cfsm

import sys, getopt
from os import listdir
from os.path import isfile, join

def analysis(inputfolder, outputfolder2, outputfolder3, outputfolder4, draw):
    files = [f for f in listdir(inputfolder) if isfile(join(inputfolder, f))]
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
    (cc3c, prefixes) = cc3closure(global_view)
    cc3res = cc3pom(cc3c, prefixes)
    cc3err = counterexamples(cc3c, cc3res)
    if draw:
        debug_graphs(cc3err, outputfolder3)
    for i in range(len(cc3err)):
        nx.readwrite.graphml.write_graphml(cc3err[i], join(outputfolder3, "%d.graphml"%i))
    projections = []
    corrected_model = global_view
    if len(cc2err) != 0:
        corrected_model = cc2c
    elif len(cc3err) != 0:
        corrected_model = cc3c
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
