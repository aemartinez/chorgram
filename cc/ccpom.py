#!/usr/bin/python

import networkx as nx
import networkx.algorithms.isomorphism as iso
from pomset import get_principal_threads, transitive_closure, get_all_principals
from pomset import get_all_prefix_graphs
from inter_closure import make_tuples, inter_process_closure
import utils
import pomset

nm = iso.categorical_node_match(["subject", "partner", "in", "out"], ["", "", "", ""])

def cc2closure(graphs):
    principals = get_all_principals(graphs[0])
    res = {}
    local_threads = get_principal_threads(graphs, principals)
    # for p in local_threads:
    #     i = 0
    #     for t in local_threads[p]:
    #         utils.debug_pomset(pomset.transitive_reduction(t), "/tmp/%s%d"%(p,i))
    #         i += 1
    tuples = make_tuples(local_threads)
    # i = 0
    # for tpl in tuples:
    #     principals = list(tpl.keys())
    #     gr = tpl[principals[0]]
    #     for p in principals[1:]:
    #         gr = nx.union(gr, tpl[p])
    #     utils.debug_pomset(pomset.transitive_reduction(gr), "/tmp/tmpl%d"%i)
    #     i += 1

    ipc = inter_process_closure(tuples, True)
    return ipc

def cc2pom(ipc, graphs):
    matches = {}
    for i in range(len(ipc)):
        matches[i] = None
        g1 = ipc[i]
        g1 = transitive_closure(g1)
        for j in range(len(graphs)):
            g2 = graphs[j]
            g3 = transitive_closure(g2)
            # m = iso.GraphMatcher(g1, g3, nm)
            # # actually should not be a subgraph
            # if m.subgraph_is_isomorphic():
            if pomset.is_more_permissive(g3, g1):
                matches[i] = j
    return matches


def get_prefixes(graphs, small):
    res_prefixes = []
    for g in graphs:
        prefixes = get_all_prefix_graphs(g, small)
        for pr in prefixes:
            toAdd = True
            for partner_g in res_prefixes:
                if (nx.is_isomorphic(pr, partner_g, node_match=nm)):
                    toAdd = False
                    break
            if toAdd:
                res_prefixes.append(pr)
    return res_prefixes

def cc3closure(graphs):
    principals = get_all_principals(graphs[0])
    local_threads = get_principal_threads(graphs, principals)
    local_prefixes = {}
    for p in principals:
        prefixes = get_prefixes(local_threads[p], False)
        local_prefixes[p] = prefixes
    tuples = make_tuples(local_prefixes)
    ipc = inter_process_closure(tuples, False)
    prefixes = get_prefixes(graphs, False)
    return (ipc, prefixes)


def cc3pom(ipc, graphs):
    matches = {}
    for i in range(len(ipc)):
        matches[i] = None
        g1 = ipc[i]
        g1 = transitive_closure(g1)
        for j in range(len(graphs)):
            if len(graphs[j].nodes()) != len(ipc[i].nodes()):
                continue
            g2 = graphs[j]
            g4 = transitive_closure(g2)
            #g4 = g2
            #m = iso.GraphMatcher(g1, g4, nm)
            # actually should not be a subgraph
            #if m.subgraph_is_isomorphic():
            if pomset.is_more_permissive(g4, g1):
                matches[i] = j
                break
    return matches

def match_export(matches):
    res = [["Graph", "Match"]] + [[i+1, matches[i] + 1 if matches[i] is not None else "error"]
                                  for i in range(len(matches.keys()))]
    return res

def counterexamples(ipc, matches):
    errors = [ipc[k] for k in matches.keys() if matches[k] is None]
    return errors

