from networkx.drawing.nx_agraph import write_dot
import networkx as nx
import os
import itertools
import shutil
from pomset import transitive_reduction

def debug_graph(gr, name="G"):
    """Some documentation"""
    write_dot(gr, '%s.dot' % name)
    lines = open('%s.dot' % name).read().split("\n")
    new_lines = []
    last_node = None
    for l in lines:
        if len(l.strip()) == 0:
            continue
        if l.strip()[0] == '"' and l.find("->") < 0:
            last_node = []
            last_node.append(l)
        elif (last_node is not None) and (l.find("];") < 0):
            last_node.append(l)
        elif (last_node is not None) and (l.find("];") > 0):
            last_node.append(l)
            subject = [l1.split("subject=")[1].split("]")[0] for l1 in last_node if l1.find("subject=") >= 0][0]
            new_lines.append("subgraph cluster_%s {" % subject)
            for l1 in last_node:
                new_lines.append(l1)
            new_lines.append("}")            
            last_node = None
        else:
            new_lines.append(l)
    open('%s.cluster.dot' % name, "w").write("\n".join(new_lines))
            
    #os.system('dot -Tpdf %s.dot -o %s.pdf' % (name, name))
    os.system('dot -Tpng %s.cluster.dot -o %s.png' % (name, name))
    return ("%s.png" % name)

def debug_graphs(grs, prefix):
    try:
        shutil.rmtree(prefix)
    except:
        pass
    os.makedirs(prefix)
    for i in range(len(grs)):
        f = debug_graph(transitive_reduction(grs[i]), "%s/graph-%d" % (prefix, i))
        print("Pomset %d\n[[file:%s]]\n" % (i+1, f))


def add_pair(gr, snd, rcv, n, msg):
    n1 = ("%s-%d" % (snd, n))
    n2 = ("%s-%d" % (rcv, n))
    gr.add_node(n1)
    gr.add_node(n2)
    gr.node[n1]['label'] = "%s%s!%s" % (snd.upper(), rcv.upper(), msg)
    gr.node[n2]['label'] = "%s%s?%s" % (snd.upper(), rcv.upper(), msg)
    gr.add_edge(n1, n2)
    return (n1, n2)


