#!/usr/bin/python3
import networkx as nx
import networkx.algorithms.isomorphism as iso

g1 = nx.readwrite.graphml.read_graphml("graphs/global.graphml")
g2 = nx.readwrite.graphml.read_graphml("graphs/1/1.graphml")

keys = ["open", "close", "sender", "receiver", "payload"]
defaults = ["" for k in keys]


nm = iso.categorical_node_match(keys, defaults)
# paths, cost = nx.algorithms.similarity.optimal_edit_paths(g1, g2, nm)
g1 = nx.subgraph(g1, list(g1.nodes()))
g2 = nx.subgraph(g2, list(g2.nodes()))

def nsc(n1, n2):
    if n1 == n2:
        return 0
    if "open" in n1 and "open" in n2:
        if n1["open"] == n2["open"]:
            return 0
        else:
            return 0.1
    if "close" in n1 and "close" in n2:
        if n1["close"] == n2["close"]:
            return 0
        else:
            return 0.1
    if "sender" in n1 and "receiver" in n1 and "payload" in n1 and \
       "sender" in n2 and "receiver" in n2 and "payload" in n2:
        if n1["sender"] == n2["sender"]:
            if n1["receiver"] == n2["receiver"]:
                if n1["payload"] == n2["payload"]:
                    return 0
                else:
                    return 0.1
            else:
                return 0.2
        else:
            return 0.3
    return 1

def ndc(n):
    return 0.45
def nic(n):
    return 0.45
def edc(n):
    return 0
def eic(n):
    return 0
        
# paths, cost = nx.algorithms.similarity.optimal_edit_paths(g1, g2, node_match=nm, node_subst_cost = nsc, node_del_cost=ndc, node_ins_cost=nic)
# paths, cost = nx.algorithms.similarity.optimal_edit_paths(g1, g2, node_ins_cost=nic, node_del_cost=ndc, node_subst_cost=nsc)
paths, cost = nx.algorithms.similarity.optimal_edit_paths(g1, g2, node_ins_cost=nic, node_del_cost=ndc, node_subst_cost=nsc, edge_del_cost=edc, edge_ins_cost=eic)


g = nx.DiGraph()


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
            if k in g2.nodes[node2]:
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
        
nx.readwrite.graphml.write_graphml(g, "graphs/res.graphml")
