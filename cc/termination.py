#!/usr/bin/python3
import networkx as nx
import networkx.algorithms.isomorphism as iso
import pomset
import os
from os import listdir
from os.path import isfile, join
from ccpom import *
import utils

keys = ["open", "close", "sender", "receiver", "payload"]

nm = iso.categorical_node_match(["subject", "partner", "in", "out"], ["", "", "", ""])


def termination_condition(global_view):
    debug = True
    
    res = {}
    principals = []
    for pom in global_view:
        for pr in get_all_principals(pom):
            if not pr in principals:
                principals.append(pr)
    errors = 0
    for p in principals:
        res[p] = []
        print("------------------------------")
        print("Principal %s" % p)
        for id_pom1 in range(len(global_view)):
            g_pom1 = global_view[id_pom1]
            for id_pom2 in range(len(global_view)):
                g_pom2 = global_view[id_pom2]
                test_path = join("/tmp", p, str(id_pom1), str(id_pom2))
                try:
                    os.makedirs(test_path)
                except:
                    pass
                
                if g_pom1 == g_pom2:
                    continue
                pom1 = pomset.transitive_closure(g_pom1)
                pom1 = pomset.proj(pom1, p)
                pom2 = pomset.transitive_closure(g_pom2)
                pom2 = pomset.proj(pom2, p)

                print(id_pom1, id_pom2)
                if debug:
                    utils.debug_pomset(pom1, join(test_path,  "pom1"))
                    utils.debug_pomset(pom2, join(test_path,  "pom2"))
                
                ext_pom = nx.DiGraph()
                for n in pom1.nodes():
                    ext_pom.add_node((1,n), **pom1.nodes[n])
                # add all missing labels of pom2
                pom2_labels = [frozenset(pom2.nodes[n].items()) for n in pom2.nodes()]
                pom1_labels = [frozenset(ext_pom.nodes[n].items()) for n in ext_pom.nodes()]
                super_set_of_labels = True
                for lbl in set(pom1_labels):
                    if pom2_labels.count(lbl) < pom1_labels.count(lbl):
                        super_set_of_labels = False
                        break
                print("Superset of labels %s" % super_set_of_labels)
                if not super_set_of_labels:
                    continue
                h = 0
                for lbl in set(pom2_labels):
                    for i in range(pom2_labels.count(lbl) - pom1_labels.count(lbl)):
                        h += 1
                        ext_pom.add_node((2,h), **(dict(lbl)))
                if debug:
                    utils.debug_pomset(ext_pom, join(test_path,  "pomext"))
                # this condition is too restrictive, since we only need to check that the
                # edge intersection is a partial order
                # m = iso.GraphMatcher(pom2, ext_pom, nm)
                m = iso.GraphMatcher(pom2, pom1, nm)
                print("Sub isomorphism %s"% m.subgraph_is_isomorphic())
                if not m.subgraph_is_isomorphic():
                    continue
                print(m.mapping)
                # this condition is broken
                #nodes = [n for n in pom2.nodes() if m.mapping[n][0] == 2]
                nodes = [n for n in pom2.nodes() if n not in m.mapping]
                print(nodes)
                pom2_sub = nx.subgraph(pom2, nodes)
                min2_sub = [n for n in pom2_sub.nodes() if len(pom2_sub.in_edges(n)) == 0]
                min2_sub = [n for n in min2_sub if "in" in pom2_sub.nodes[n]]
                if len(min2_sub) == 0:
                    continue
                res[p].append((id_pom1, id_pom2,
                               pom1,
                               pom2,
                               dict([(m.mapping[k], k) for k in m.mapping]),
                               min2_sub       
                ))
                print([pom2_sub.nodes[n] for n in min2_sub])
                if debug:
                    export_termination_counterexample("/tmp",*res[p][-1])

    return res

def get_event_label(event):
    label = None
    if "in" in event:
        label = "%s%s?%s"%(
            event["partner"],
            event["subject"],
            event["in"]
        )
    if "out" in event:
        label = "%s%s!%s"%(
            event["subject"],
            event["partner"],
            event["out"]
        )
    return label
    
def export_termination_counterexample(path, id_pom1, id_pom2,
                                      pom1, pom2,
                                      mapping, mins):
    name = join(path, "%d-%d"%(id_pom1, id_pom2))
    try:
        os.makedirs(path)
    except:
        pass

    pom1 = pomset.transitive_reduction(pom1)
    pom2 = pomset.transitive_reduction(pom2)
    f = open("%s.dot"%name, "w")
    f.write("""
strict digraph "" {
	graph [edge_default="{}",
		node_default="{}"
	];
	node [label="\\N"];
""")
    for n in pom1.nodes():
        label = get_event_label(pom1.nodes[n])
        f.write('subgraph cluster_%d {A_%s\t[label="%s"];}\n'%(1, n, label))
    for (e1, e2) in pom1.edges():
        f.write('A_%s -> A_%s;\n'%(e1, e2))

    for n in pom2.nodes():
        label = get_event_label(pom2.nodes[n])
        f.write('subgraph cluster_%d {B_%s\t[label="%s"'%(2, n, label))
        if n in mins:
            f.write('color=darkolivegreen3')
        f.write('];}\n')
    for (e1, e2) in pom2.edges():
        f.write('B_%s -> B_%s;\n'%(e1, e2))

    for m in mapping:
        f.write('A_%s -> B_%s [style=dashed];\n'%(m, mapping[m]))
    f.write('}\n')
    f.close()
    os.system('dot -Tpng %s.dot -o %s.png' % (name, name))
    return

if __name__ == "__main__":
    inputfolder = "global_view"
    files = [f for f in listdir(inputfolder) if isfile(join(inputfolder, f))]
    files = [f for f in files if f.split(".")[-1] == "graphml"]
    global_view = [nx.readwrite.graphml.read_graphml(join(inputfolder, f)) for f in files]
    res = termination_condition(global_view)
    for p in res:
        for c in res[p]:
            export_termination_counterexample(join("/tmp", p), *c)
    exit(0)
