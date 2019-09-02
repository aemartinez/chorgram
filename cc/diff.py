#!/usr/bin/python3
import networkx as nx
import networkx.algorithms.isomorphism as iso

g1 = nx.readwrite.graphml.read_graphml("global_view/global_view0.graphml")
g2 = nx.readwrite.graphml.read_graphml("global_view_corrected/0.graphml")

nm = iso.categorical_node_match(["subject", "partner", "in", "out"], ["", "", "", ""])
paths, cost = nx.algorithms.similarity.optimal_edit_paths(g1, g2, nm)

g = nx.DiGraph()

keys = ["subject", "partner", "in", "out"]

reverse_map = {}
for p in paths[0][0]:
    (node1, node2) = p
    if (not node1 is None) and  (not node2 is None):
        attrs = {}
        for k in keys:
            if k in g1.nodes[node1]:
                attrs[k] = g1.nodes[node1][k]
                if not k in g2.nodes[node2]:
                    attrs[k + "-remove"] = "1"
                elif g1.nodes[node1][k] != g2.nodes[node2][k]:
                    attrs[k + "-change-to"] = g2.nodes[node2][k]
            elif k in g2.nodes[node2]:
                attrs[k + "-add"] = g2.nodes[node2][k]
        g.add_node(node1, **attrs)
        reverse_map[node2] = node1
    elif (not node1 is None) and  (node2 is None):
        attrs = {}
        for k in keys:
            if k in g1.nodes[node1]:
                attrs[k] = g1.nodes[node1][k]
        attrs["deleted"] = "1"
        g.add_node(node1, **attrs)
    else:
        attrs = {}
        for k in keys:
            if k in g1.nodes[node2]:
                attrs[k] = g2.nodes[node2][k]
        attrs["added"] = "1"
        g.add_node(node2, **attrs)

for (e1, e2) in paths[0][1]:
    if (not e1 is None) and (not e2 is None):
        g.add_edge(*e1)
    elif (not e1 is None) and (e2 is None):
        g.add_edge(*e1, deleted="1")
    elif (e1 is None) and (not e2 is None):
        (n1,n2) = e2
        if n1 in reverse_map:
            n1 = reverse_map[n1]
        if n2 in reverse_map:
            n2 = reverse_map[n2]
        g.add_edge(n1, n2, added="1")
        
nx.readwrite.graphml.write_graphml(g, "/tmp/res.graphml")
