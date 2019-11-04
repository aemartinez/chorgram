import networkx as nx
import itertools
from pomset import get_all_receive_lbls, get_all_send_lbls, get_matching_label
from pomset import proj_lbl
from pomset import transitive_closure
from pomset import linearizations
import utils
import pomset

import networkx.algorithms.isomorphism as iso

nm = iso.categorical_node_match(["subject", "partner", "in", "out"], ["", "", "", ""])


def make_tuples(local_threads):
    principals = local_threads.keys()
    tuples = []
    elems = [[(p, t) for t in local_threads[p]] for p in principals]
    gr_tuples = list(itertools.product(*elems))
    for gr in gr_tuples:
        tpl = {}
        for e in gr:
            tpl[e[0]] = e[1]
        tuples.append(tpl)
    return tuples

j = 0
# several operaions can be optimized. Bu we do not care.
def join_graph(gr, label):
    gps = []
    gr = transitive_closure(gr)
    gr1 = proj_lbl(gr, label)
    gr2 = proj_lbl(gr, get_matching_label(label))

    lbl1 = dict(label)
    if "in" in lbl1 and len(gr1.nodes()) > len(gr2.nodes()):
        return None
    if "out" in lbl1 and len(gr1.nodes()) < len(gr2.nodes()):
        return None

    
    all_lin1 = linearizations(gr1)
    all_lin2 = linearizations(gr2)
    
    for (lin1, lin2) in itertools.product(all_lin1, all_lin2):
        #if len(lin1) != len(lin2):
        #    print "number of events do not match"
        #    return None
        new_gr = gr.copy()
        for i in range(min(len(lin1), len(lin2))):
            ev1 = lin1[i]
            ev2 = lin2[i]
            new_gr.add_edge(ev2, ev1)
        new_gr = transitive_closure(new_gr)
        if len(list(nx.simple_cycles(new_gr))) == 0:
            gps.append(new_gr)
    return gps


def join_graphs(gps, label):
    gps1 = []
    for gr in gps:
        new_graphs = join_graph(gr, label)
        if new_graphs is None:
            return None
        gps1 += new_graphs
    return gps1


def inter_process_closure(tuples, complete):
    j = 0
    ipc = []
    for tpl in tuples:
        principals = list(tpl.keys())
        gr = tpl[principals[0]]
        for p in principals[1:]:
            gr = nx.union(gr, tpl[p])
        grs = [gr]
        # important this is done on receive,
        # or we will not remove unmatched events
        inputs = get_all_receive_lbls(gr)
        outputs = get_all_send_lbls(gr)
        is_complete = True
        not_matched_out = []
        for o in outputs:
            if len(proj_lbl(gr, o).nodes()) != len(proj_lbl(gr, get_matching_label(o)).nodes()):
                is_complete = False
                not_matched_out.append(o)
        if not is_complete:
            if complete:
                continue
            #else:
            #    print "Not complete", not_matched_out

        step = 0
        for l in inputs:
            grs = join_graphs(grs, l)
            if grs is None:
                break
            i = 0
            for gr1 in grs:
                # utils.debug_pomset(gr1, "/tmp/ipc-tmp-%d-%d-%d"%(j, step, i))
                i+=1
            step += 1
        i = 0
        if grs is not None:
            ipc = ipc + grs
            for new_gr in grs:
                # utils.debug_pomset(pomset.transitive_reduction(new_gr), "/tmp/ipc%d-%d"%(j,i))
                i+=1
        j+=1


    res = []
    for g in ipc:
        toAdd = True
        for partner_g in res:
            if (nx.is_isomorphic(g, partner_g, node_match=nm)):
                toAdd = False
                break
        if toAdd:
            res.append(g)
    return res
