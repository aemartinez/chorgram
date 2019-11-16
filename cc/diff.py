#!/usr/bin/python3
import networkx as nx
import networkx.algorithms.isomorphism as iso
import pomset

keys = ["open", "close", "sender", "receiver", "payload"]

def cost_functions(costs):
   def nsc(n1, n2):
       if n1 == n2:
           return 0
       if "open" in n1 and "open" in n2:
           if n1["open"] == n2["open"]:
               return 0
           else:
               return costs["change_open_gate"]
       if "close" in n1 and "close" in n2:
           if n1["close"] == n2["close"]:
               return 0
           else:
               return costs["change_close_gate"]
       if "sender" in n1 and "receiver" in n1 and "payload" in n1 and \
          "sender" in n2 and "receiver" in n2 and "payload" in n2:
           if n1["sender"] == n2["sender"]:
               if n1["receiver"] == n2["receiver"]:
                   if n1["payload"] == n2["payload"]:
                       return 0
                   else:
                       return costs["change_payload"]
               else:
                   # return 0.2
                   return costs["change_receiver"]
           else:
               # return 0.3
               return costs["change_sender"]
       return 1

   def ndc(n):
       return costs["delete_node"]
   def nic(n):
       return costs["insert_node"]
    
   def edc(n):
       return costs["delete_edge"]
   def eic(n):
       return costs["insert_edge"]

   return (nsc, ndc, nic, edc, eic)

def is_source_node(attr):
    return "open" in attr and attr["open"] == "Source"
def is_branch_node(attr):
    return "open" in attr and attr["open"] == "Branch"
def is_merge_node(attr):
    return "close" in attr and attr["close"] == "Merge"
def is_sink_node(attr):
    return "close" in attr and attr["close"] == "Sink"

def gen_top_choices(src):
    source_node = [n for n in src.nodes() if is_source_node(src.nodes[n])][0]
    sink_node = [n for n in src.nodes() if is_sink_node(src.nodes[n])][0]
    branch_nodes = [n for n in src.nodes() if is_branch_node(src.nodes[n])]
    merge_nodes = [n for n in src.nodes() if is_merge_node(src.nodes[n])]

    gtr = nx.transitive_closure(src)
    branches_gr = nx.subgraph(gtr, branch_nodes + merge_nodes)
    paths = []
    min_branches = [b for b in branch_nodes if len(branches_gr.in_edges(b)) == 0]

    for branch in min_branches:
        for (b, choice) in src.out_edges(branch):
            new_graph = nx.DiGraph(src)

            for (in_node, b1) in new_graph.in_edges(branch):
                new_graph.add_edge(in_node, choice, branch_src = branch)
            new_graph = nx.DiGraph(nx.subgraph(new_graph, [n for n in new_graph.nodes() if n != branch]))
            matching_merges = [m for (a,m) in branches_gr.out_edges(branch) if m in merge_nodes and len(branches_gr.out_edges(m)) == 0]
            path = {branch: (choice, matching_merges)}
            for merge in matching_merges:
                for (in_node, a) in new_graph.in_edges(merge):
                    for (a, out_node) in new_graph.out_edges(merge):
                        new_graph.add_edge(in_node, out_node, merge_src = merge)
                new_graph = nx.subgraph(new_graph, [n for n in new_graph.nodes() if n != merge])
            new_graph_tr = nx.transitive_closure(new_graph)
            reachable = [x for (a,x) in new_graph_tr.out_edges(source_node)]+[source_node]
            new_graph = nx.subgraph(new_graph, reachable)
            paths.append((path, new_graph))
    return paths

def gen_all_choices(src):
    to_process = [({}, src)]
    res = []
    while len(to_process) > 0:
        (path, graph) = to_process.pop()
        branch_nodes = [n for n in graph.nodes() if is_branch_node(graph.nodes[n])]
        if len(branch_nodes) == 0:
            res.append((path, graph))
            continue
        paths = gen_top_choices(graph)
        to_process = to_process + [(dict(list(path.items()) + list(p.items())), g) for (p, g) in paths]
    # i = 0
    # for (p, g) in res:
    #     nx.readwrite.graphml.write_graphml(g, "graphs/aaa%d.graphml" % i)
    #     i += 1
    return res

def from_diff_to_graph(g1, g2, g3, diffs, brs):
    g = nx.DiGraph()
    for n in g1.nodes():
        g.add_node((1,n), **g1.nodes[n])
    reverse_map = {}
    for p in diffs[0][0]:
        (node1, node2) = p
        if (not node1 is None) and  (not node2 is None):
            n = (1,node1)
            changed = False
            for k in keys:
                if k in g1.nodes[node1]:
                    if not k in g2.nodes[node2]:
                        g.nodes[n][k + "-remove"] = "1"
                        changed = True
                    elif g1.nodes[node1][k] != g2.nodes[node2][k]:
                        g.nodes[n][k + "-change-to"] = g2.nodes[node2][k]
                        changed = True
                elif k in g2.nodes[node2]:
                    g.nodes[n][k + "-add"] = g2.nodes[node2][k]
                    changed = True
            if changed:
                g.nodes[n]["changed"] = "1"
            else:
                g.nodes[n]["kept"] = "1"
            g.nodes[n]["src-node"] = node1
            g.nodes[n]["target-node"] = node2
            g.nodes[n]["node-id"] = "%s-%s"%((1,node1))
            reverse_map[node2] = node1
        elif (not node1 is None) and  (node2 is None):
            n = (1,node1)
            g.nodes[n]["deleted"] = "1"
            g.nodes[n]["src-node"] = node1
            g.nodes[n]["node-id"] = "%s-%s"%((1,node1))
        else:
            attrs = {}
            for k in keys:
                if k in g2.nodes[node2]:
                    attrs[k] = g2.nodes[node2][k]
            attrs["added"] = "1"
            attrs["target-node"] = node2
            attrs["node-id"] = "%s-%s"%((2,node2))
            g.add_node((2,node2), **attrs)

    # keep selected branches and merges
    all_mething_merges = []
    for b in brs:
        g.nodes[(1,b)]["kept"] = "1"
        all_mething_merges += brs[b][1]
    for m in all_mething_merges:
        g.nodes[(1,m)]["kept"] = "1"

    # first we add edges for every node that has not been chosen
    # g3 is the subchoice choreography
    for n in g1.nodes():
        if not n in g3.nodes() and \
           not n in brs and \
           not n in all_mething_merges:
            for (n1,n2) in g1.in_edges(n):
                g.add_edge((1, n1), (1,n2))
            for (n1,n2) in g1.out_edges(n):
                g.add_edge((1, n1), (1,n2))
    # we add all edges that have been preserved
    for (e1, e2) in diffs[0][1]:
        if (not e1 is None) and (not e2 is None):
            # we must take into account that some edges come by connecting nodes after we removed branches
            if (e1[0], e1[1]) in g1.out_edges():
                g.add_edge((1,e1[0]), (1,e1[1]))
                g.edges[(1,e1[0]), (1,e1[1])]["kept"]="1"
    # reconnect branches and merges for the selected choice
    old_order = nx.transitive_closure(g1)
    new_order = nx.transitive_closure(g2)
    for branch in brs:
        merge = brs[branch][1][0]
        print(branch, merge)
        # connect branch
        old_before = [a for (a,x) in old_order.in_edges(branch)]
        old_in_branch = [b for (x,b) in old_order.out_edges(branch) if not (merge, b) in old_order.out_edges(merge) and b != merge]
        old_after = [b for (x,b) in old_order.out_edges(merge)]
        old_out_branch = old_before + old_after

        # Problem is we cannot move messages across branches
        new_in_branch = [a1 for a1 in new_order.nodes() if a1 in reverse_map and reverse_map[a1] in old_in_branch]
        new_out_branch = [a1 for a1 in new_order.nodes() if a1 in reverse_map and reverse_map[a1] in old_out_branch]

        new_internal_order = nx.subgraph(new_order, new_in_branch)
        internal_mins = [a for a in new_in_branch if len(new_internal_order.in_edges(a)) == 0]
        internal_maxs = [a for a in new_in_branch if len(new_internal_order.out_edges(a)) == 0]

        def add_edge_and_attr(s, t):
            g.add_edge((1, s), (1,t))
            if (s, t) in g1.out_edges():
                g.edges[(1, s), (1,t)]["kept"]="1"
            else:
                g.edges[(1, s), (1,t)]["added"]="1"
        
        for n in internal_mins:
            add_edge_and_attr(branch, reverse_map[n])
        for n in internal_maxs:
            add_edge_and_attr(reverse_map[n], merge)

        new_external_preds = [n for n in new_out_branch if len([(a,b) for (a,b) in new_order.out_edges(n) if b in new_in_branch]) != 0]
        new_external_succs = [n for n in new_out_branch if len([(a,b) for (a,b) in new_order.in_edges(n) if a in new_in_branch]) != 0]
        new_pred_order = nx.subgraph(new_order, new_external_preds)
        external_maxs = [a for a in new_external_preds if len(new_pred_order.out_edges(a)) == 0]
        for n in external_maxs:
            add_edge_and_attr(reverse_map[n], branch)

        new_succ_order = nx.subgraph(new_order, new_external_succs)
        external_mins = [a for a in new_external_succs if len(new_succ_order.in_edges(a)) == 0]
        for n in external_mins:
            add_edge_and_attr(merge, reverse_map[n])
    # connect the missing edges
    temp_order = nx.transitive_closure(g)
    for (n1,n2) in g2.edges():
        node1 = (2,n1)
        node2 = (2,n2)
        if n1 in reverse_map:
            node1 = (1,reverse_map[n1])
        if n2 in reverse_map:
            node2 = (1,reverse_map[n2])
        if not (node1, node2) in temp_order.out_edges(node1):
            g.add_edge(node1, node2)
            g.edges[node1,node2]["added"]="1"
    for (n1,n2) in g1.edges():
        if not ((1,n1), (1, n2)) in g.edges():
            g.add_edge((1,n1), (1, n2))
            g.edges[(1,n1), (1, n2)]["deleted"]="1"
    return g


def clone_graph(g):
    g1 = nx.DiGraph()
    node_mapping = {}
    iNode = 0
    for n in g.nodes():
        node_mapping[n] = iNode
        g1.add_node(iNode, **g.nodes[n])
        iNode +=1
    for (n1,n2) in g.edges():
        g1.add_edge(node_mapping[n1], node_mapping[n2], **g.edges[n1,n2])
    return g1


def run_diff(g1, g2, folder, costs):
    res = {}
    paths = gen_all_choices(g1)

    (nsc, ndc, nic, edc, eic) = cost_functions(costs)
    for i in range(len(paths)):
        (path, new_graph) = paths[i]
        diffs, cost = nx.algorithms.similarity.optimal_edit_paths(new_graph, g2, node_ins_cost=nic, node_del_cost=ndc, node_subst_cost=nsc, edge_del_cost=edc, edge_ins_cost=eic)
        print(diffs, cost)
        g = from_diff_to_graph(g1, g2, new_graph, diffs, path)
        g = clone_graph(g)
        nx.readwrite.graphml.write_graphml(g, folder+"/diff_%d.graphml" % i)
        res[i] = cost
    return res

if __name__ == "__main__":
    g1 = nx.readwrite.graphml.read_graphml("choreography.graphml")
    g2 = nx.readwrite.graphml.read_graphml("cc2/synthesis/1/1.graphml")
    run_diff(g1, g2, "graphs")
