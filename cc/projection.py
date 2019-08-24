import networkx as nx
from pomset import mins, proj

def attr_to_label(attr):
    if "in" in attr:
        return("%s%s?%s" % (attr["partner"], attr["subject"], attr["in"]))
    else:
        return("%s%s!%s" % (attr["subject"], attr["partner"], attr["out"]))

def proj_to_cfsm(graphs, pr):
    automaton = nx.DiGraph()
    pr_pomsets = [proj(gr, pr) for gr in graphs]
    
    id_to_pomsets = {}
    nid = 0
    root = nid
    id_to_pomsets[nid] = pr_pomsets
    
    attr = {"initial": 1}
    automaton.add_node(nid, attr)
    
    
    to_process = [root]
    
    while (len(to_process) > 0):
       state = to_process.pop()
       l_to_nodes = {}
       for gr in id_to_pomsets[state]:
           nodes = mins(gr)
           for n in nodes:
               lbl = frozenset(gr.node[n].items())
               if not lbl in l_to_nodes:
                   l_to_nodes[lbl] = []
               l_to_nodes[lbl].append((gr, n))
    
       for l in l_to_nodes.keys():
           nid +=1
           to_process.append(nid)
           automaton.add_node(nid)
           attr = dict(l)
           attr["label"] = attr_to_label(attr)
           automaton.add_edge(state, nid, attr)
           target_pomsets = []
           for (gr, node) in l_to_nodes[l]:
               nodes = [x for x in gr.nodes() if x != node]
               target_pomsets.append(nx.subgraph(gr, nodes))
               id_to_pomsets[nid] = target_pomsets
    return automaton
