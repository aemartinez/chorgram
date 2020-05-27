import networkx as nx
import networkx.algorithms.isomorphism as iso

labels = ["subject", "partner", "in", "out"]
nm = iso.categorical_node_match(labels, ["" for l in labels])

def clone_node_attr(n1, n2):
    for attr in ["subject", "partner", "in", "out"]:
        if attr in n1:
            n2[attr] = n1[attr]

def transitive_closure(gr):
    gr1 = nx.transitive_closure(gr)
    for n in gr1.nodes():
        clone_node_attr(gr.nodes[n], gr1.nodes[n])
    return gr1

def transitive_reduction(gr):
    gr = nx.subgraph(gr, gr.nodes())
    gr = nx.DiGraph(gr)
    for (n1, n2) in list(gr.edges()):
        common = set([n for n in nx.descendants(gr, n1) if not n in [n1,n2]]).\
            intersection(
                set([n for n in nx.ancestors(gr, n2) if not n in [n1,n2]]))
        if len(common) > 0:
            gr.remove_edge(n1,n2)
    return gr

def proj(gr, pr):
    nodes = [x for x in gr.nodes() if gr.nodes[x]["subject"] == pr]
    return nx.subgraph(gr, nodes)

def proj_lbl(gr, label):
    nodes = [x for x in gr.nodes() if frozenset(gr.nodes[x].items()) == label]
    return nx.subgraph(gr, nodes)


def map_lbls(gr):
    """ Returns a map label->nodes """
    lbls = {}
    for n in gr.nodes():
        lbl = gr.nodes[n]["label"]
        if lbl in lbls:
            lbls[lbl].append(n)
        else:
            lbls[lbl] = [n]
    return lbls

def get_principal_threads(graphs, principals):
    local_threads = {}
    for p in principals:
        gps = []
        i = 0
        for g in graphs:
            prog_gr = proj(g, p)
            toAdd = True
            for partner_g in gps:
                if (nx.is_isomorphic(prog_gr, partner_g, node_match=nm)):
                    toAdd = False
                    break
            if toAdd:
                gps.append(prog_gr)
            i += 1
        local_threads[p] = gps
    return local_threads

def get_matching_label(lbl):
    lbl1 = dict(lbl)
    if "in" in lbl1:
        return frozenset({"out": lbl1["in"], "subject": lbl1["partner"], "partner": lbl1["subject"]}.items())
    else:
        return frozenset({"in": lbl1["out"], "subject": lbl1["partner"], "partner": lbl1["subject"]}.items())

def get_all_send_lbls(gr):
    lbls = set()
    for n in gr.nodes():
        if not "out" in gr.nodes[n]:
            continue
        lbls.add(frozenset(gr.nodes[n].items()))
    return lbls
def get_all_receive_lbls(gr):
    lbls = set()
    for n in gr.nodes():
        if not "in" in gr.nodes[n]:
            continue
        lbls.add(frozenset(gr.nodes[n].items()))
    return lbls
def get_all_send_nodes(gr):
    nodes = set()
    for n in gr.nodes():
        lbl = gr.nodes[n]["label"]
        if lbl[2] == "!":
            nodes.add(n)
    return nodes
def get_all_receive_nodes(gr):
    nodes = set()
    for n in gr.nodes():
        lbl = gr.nodes[n]["label"]
        if lbl[2] == "?":
            nodes.add(lbl)
    return nodes
def get_all_principals(gr):
    l =  list(set([gr.nodes[n]["subject"] for n in gr.nodes()]))
    l.sort()
    return l

def mins(gr):
    return [n for n in gr.nodes() if len(gr.in_edges(n)) == 0]


def linearizations(gr):
    to_process = [([], gr.nodes())]
    words = []
    while (len(to_process) > 0):
        (s, nodes) = to_process.pop()
        for n in nodes:
            deps = [a for (a, b) in gr.in_edges(n)]
            inter = set(nodes).intersection(set(deps))
            if (len(inter) > 0):
                continue
            nodes1 = [n1 for n1 in nodes]
            nodes1.remove(n)
            s1 = [a for a in s]
            s1.append(n)
            if len(nodes1) == 0:
                words.append(s1)
            else:
                to_process.append((s1, nodes1))
    return words


def get_all_prefixes(gr, small):
    nodes = set(gr.nodes())
    to_process = set()
    to_process.add(frozenset())
    prefixes = []

    while (len(to_process) > 0):
        to_add = []
        prefix = to_process.pop()
        for n in nodes.difference(prefix):
            deps = set([a for (a, b) in gr.in_edges(n)])
            if len(deps.difference(prefix)) > 0:
                continue
            if small:
                to_add.append(n)
            else:
                to_process.add(frozenset(prefix.union([n])))
        if small:
            if len(to_add) > 0:
                to_process.add(frozenset(prefix.union(to_add)))
        prefixes.append(prefix)
    return prefixes


def get_all_prefix_graphs(gr, small):
    prefixes = get_all_prefixes(gr, small)
    return [nx.subgraph(gr, pr) for pr in prefixes]


def get_line_graph(pom):
    p = nx.generators.line.line_graph(pom)
    for (n1, n2) in p.nodes():
        for l in labels:
            if l in pom.nodes[n1]:
                p.nodes[(n1, n2)]["s-%s"%l] = pom.nodes[n1][l]
            if l in pom.nodes[n2]:
                p.nodes[(n1, n2)]["t-%s"%l] = pom.nodes[n2][l]
    return p

def is_more_permissive(pom1, pom2):
    p1 = get_line_graph(pom1)
    p2 = get_line_graph(pom2)
    labels1 = ["s-%s"%l for l in ["subject", "partner", "in", "out"]] + \
        ["t-%s"%l for l in ["subject", "partner", "in", "out"]]
    nm1 = iso.categorical_node_match(labels1, ["" for l in labels1])
    m1 = iso.DiGraphMatcher(p2, p1, nm1)
    if not m1.subgraph_is_isomorphic():
        return False
    #pom1 edges a subset of pom2 edges
    pom1_labels = [frozenset(pom1.nodes[n].items()) for n in pom1.nodes()]
    pom2_labels = [frozenset(pom2.nodes[n].items()) for n in pom2.nodes()]
    if set(pom1_labels) != set(pom2_labels):
        return False
    for lbl in set(pom1_labels):
        if pom2_labels.count(lbl) != pom1_labels.count(lbl):
            return False
    return True
