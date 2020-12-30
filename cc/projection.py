import networkx as nx
from pomset import mins, proj
from os.path import isfile, join
import os

def attr_to_label(attr):
    if "in" in attr:
        return("%s %s?%s" % (attr["partner"], attr["subject"], attr["in"]))
    else:
        return("%s %s!%s" % (attr["subject"], attr["partner"], attr["out"]))

def is_state_terminating(pomsets):
    for pom in pomsets:
        if len(pom.nodes()) == 0:
            return True
    return False

def proj_to_cfsm(graphs, pr):
    automaton = nx.DiGraph()
    pr_pomsets = [proj(gr, pr) for gr in graphs]
    
    id_to_pomsets = {}
    nid = 0
    root = nid
    id_to_pomsets[nid] = pr_pomsets
    
    attr = {"initial": 1,
            "terminating": is_state_terminating(id_to_pomsets[nid])}
    automaton.add_node(nid, **attr)
    
    
    to_process = [root]
    while (len(to_process) > 0):
       state = to_process.pop()
       l_to_nodes = {}
       for gr in id_to_pomsets[state]:
           nodes = mins(gr)
           for n in nodes:
               lbl = frozenset(gr.nodes[n].items())
               if not lbl in l_to_nodes:
                   l_to_nodes[lbl] = []
               l_to_nodes[lbl].append((gr, n))
    
       for l in l_to_nodes.keys():
           nid +=1
           to_process.append(nid)
           automaton.add_node(nid)
           attr = dict(l)
           attr["label"] = attr_to_label(attr)
           automaton.add_edge(state, nid, **attr)
           target_pomsets = []
           for (gr, node) in l_to_nodes[l]:
               nodes = [x for x in gr.nodes() if x != node]
               target_pomsets.append(nx.subgraph(gr, nodes))
               id_to_pomsets[nid] = target_pomsets
           automaton.nodes[nid]["terminating"] = is_state_terminating(id_to_pomsets[nid])
    return automaton

def export_projection(path, id_participant, projection):
    name = join(path, "%s"%(id_participant))
    try:
        os.makedirs(path)
    except:
        pass

    f = open("%s.dot"%name, "w")
    f.write("""
strict digraph "" {
	graph [edge_default="{}",
		node_default="{}"
	];
	node [label="\\N"];
""")
    for n in projection.nodes():
        attrs = 'label="%s"' % n
        if "terminating" in projection.nodes[n] and projection.nodes[n]["terminating"]:
            attrs+= ';style=dashed'
        f.write('node_%s\t[%s];\n'%(n, attrs))
    for (e1, e2) in projection.edges():
        label = attr_to_label(projection.edges[(e1, e2)])
        f.write('node_%s -> node_%s \t[label="%s"];\n'%(e1, e2,label))
    f.write('}\n')
    f.close()
    os.system('dot -Tpng %s.dot -o %s.png' % (name, name))
    return
