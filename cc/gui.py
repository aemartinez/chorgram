#!/usr/bin/python3

import os
from os import listdir
from os.path import isfile, join
import glob
import gi

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk, Gdk
import subprocess
import networkx as nx
import utils
import shutil
import pomset
import termination
from ccpom import *
from diff import run_diff
from projection import proj_to_cfsm, export_projection

# Quick patch to disregard which folder the gui script is run from
from pathlib import Path
os.environ['PATH'] += os.path.pathsep + \
   str(Path(__file__).parent.absolute().parent)

class Workspace:
    def __init__(self, sgg_path):
        self.sgg_absolute_path = sgg_path
        self.semantics = None
        self.cc2 = None
        self.cc3 = None
        self.termination = None
        self.projections = None

    def get_root_folder(self):
        return join(
            os.path.dirname(self.sgg_absolute_path),
            os.path.basename(self.sgg_absolute_path).split('.')[0]
        )

    def gen_choreography_graphml(self):
        os.system(
            "gc2gml %s > %s"
            % (self.sgg_absolute_path, self.get_root_folder() + "/choreography.graphml")
        )

    def get_choreography_png_path(self):
        return self.get_root_folder() + "/choreography.png"

    def gen_choreography_png(self):
        if not os.path.exists(self.get_root_folder()):
            os.makedirs(self.get_root_folder())
        self.gen_choreography_graphml()
        os.system(
            "gc2dot -d %s/ --fmt sloppygml %s/choreography.graphml"
            % (self.get_root_folder(), self.get_root_folder())
        )
        os.system(
            "dot -Tpng %s/choreography.dot -o %s"
            % (self.get_root_folder(), self.get_choreography_png_path())
        )

    def get_semantics_folder(self):
        return self.get_root_folder()

    def list_files_in_folder(self, folder):
        files = [f for f in listdir(folder) if isfile(join(folder, f))]
        return files

    def get_semantics_png_path(self, f):
        return "/%s.png" % f

    def delete_folder(self, folder):
        try:
            shutil.rmtree(folder)
        except:
            pass

    def gen_semantics(self):
        # I should ensure this goes in the third position and remove all the past semantics from the tree
        for f in glob.glob(join(self.get_semantics_folder() + '*.graphml')) + \
            glob.glob(join(self.get_semantics_folder() + '*.dot')):
            os.remove(f) # Cleanup old semantics

        cmd = "gc2pom -d %s/ --gml %s" % (
            os.path.dirname(self.sgg_absolute_path),
            self.sgg_absolute_path,
        )
        os.system(cmd)
        self.semantics = {}
        for f in glob.glob(join(self.get_semantics_folder(), '*.graphml')):
            if f.endswith('choreography.graphml'):
                continue
            graph = nx.readwrite.graphml.read_graphml(
                join(self.get_semantics_folder(), f)
            )
            self.semantics[f] = graph
            utils.debug_pomset(
                pomset.transitive_reduction(graph), join(self.get_semantics_folder(), f)
            )

    def get_cc2_folder(self):
        return self.get_root_folder() + "/cc2"

    def gen_cc2(self):
        if self.semantics is None:
            return
        self.delete_folder(self.get_cc2_folder())
        os.makedirs(join(self.get_cc2_folder(), "closure"))
        os.makedirs(join(self.get_cc2_folder(), "synthesis"))

        pomsets = [self.semantics[f] for f in self.semantics]
        cc2c = cc2closure(pomsets)
        self.cc2 = {"closure": {}, "mapping": {}}
        cc2res = cc2pom(cc2c, pomsets)
        i = 0
        for pm in cc2c:
            # TODO: we should use the transitive reduction, but it does not work

            nx.readwrite.graphml.write_graphml(
                pm, join(self.get_cc2_folder(), "closure", "%d.graphml" % i)
            )
            utils.debug_pomset(
                pomset.transitive_reduction(pm),
                join(self.get_cc2_folder(), "closure", "%d" % i),
            )
            self.cc2["closure"][i] = pm
            if not cc2res[i] is None:
                self.cc2["mapping"][i] = cc2res[i]
            i += 1

    def get_cc2_closure_png_path(self, i):
        return join(self.get_cc2_folder(), "closure", "%d.png" % i)

    def get_cc2_counter_choreography_folder(self, pm_idx):
        return join(self.get_cc2_folder(), "synthesis", "%d" % pm_idx)

    def get_cc2_counter_choreography_png(self, pm_idx):
        return join(self.get_cc2_counter_choreography_folder(pm_idx), "%d.png" % pm_idx)

    def get_cc3_closure_png_path(self, i):
        return join(self.get_cc3_folder(), "closure", "%d.png" % i)

    def get_cc3_counter_choreography_folder(self, pm_idx):
        return join(self.get_cc3_folder(), "synthesis", "%d" % pm_idx)

    def get_cc3_counter_choreography_png(self, pm_idx):
        return join(self.get_cc3_counter_choreography_folder(pm_idx), "%d.png" % pm_idx)

    def gen_cc2_choreography(self, pm_idx):
        if self.cc2 is None:
            return
        if not pm_idx in self.cc2["closure"]:
            return
        pm = self.cc2["closure"][pm_idx]
        os.system(
            "pom2gg -d %s %s"
            % (
                join(self.get_cc2_folder(), "synthesis"),
                join(self.get_cc2_folder(), "closure", "%d.graphml" % pm_idx),
            )
        )
        os.system(
            "dot -Tpng %s.dot -o %s"
            % (
                join(self.get_cc2_counter_choreography_folder(pm_idx), "%d" % pm_idx),
                self.get_cc2_counter_choreography_png(pm_idx),
            )
        )
        return os.path.isfile(self.get_cc2_counter_choreography_png(pm_idx))

    def get_cc2_diff_path(self, counter_idx, branch_idx):
        return "%s/diff_%d.png" % (
            self.get_cc2_counter_choreography_folder(counter_idx),
            branch_idx,
        )

    def gen_cc2_diff(self, pm_idx, costs):
        if self.cc2 is None:
            return
        if not pm_idx in self.cc2["closure"]:
            return
        pm = self.cc2["closure"][pm_idx]
        g1 = nx.readwrite.graphml.read_graphml(
            self.get_root_folder() + "/choreography.graphml"
        )
        g2 = nx.readwrite.graphml.read_graphml(
            join(
                self.get_cc2_folder(), "synthesis", "%d" % pm_idx, "%d.graphml" % pm_idx
            )
        )
        res = run_diff(g1, g2, self.get_cc2_counter_choreography_folder(pm_idx), costs)
        for i in res:
            os.system(
                "gc2dot -d %s/ --fmt gmldiff %s/diff_%d.graphml"
                % (
                    self.get_cc2_counter_choreography_folder(pm_idx),
                    self.get_cc2_counter_choreography_folder(pm_idx),
                    i,
                )
            )
            os.system(
                "dot -Tpng %s/diff_%d.dot -o %s"
                % (
                    self.get_cc2_counter_choreography_folder(pm_idx),
                    i,
                    self.get_cc2_diff_path(pm_idx, i),
                )
            )
        return res

    def get_cc3_folder(self):
        return self.get_root_folder() + "/cc3"

    def get_cc3_diff_path(self, counter_idx, branch_idx):
        return "%s/diff_%d.png" % (
            self.get_cc3_counter_choreography_folder(counter_idx),
            branch_idx,
        )

    # remove closure elements that are prefix of other elements
    def filter_cc3_closure(self, cc3c):
        filtered_closure = []
        nm = iso.categorical_node_match(
            ["subject", "partner", "in", "out"], ["", "", "", ""]
        )
        for pomset in cc3c:
            found = False
            for pomset1 in cc3c:
                if pomset1 == pomset:
                    continue
                prefixes = get_all_prefix_graphs(pomset1, False)
                for pomset2 in prefixes:
                    if nx.is_isomorphic(pomset, pomset2, node_match=nm):
                        found = True
                        break
                if found:
                    break
            if not found:
                filtered_closure.append(pomset)
        return filtered_closure

    def fix_cc3_counter_example(self, pom):
        last_int = max([int(x) for x in pom.nodes()])
        for node in list(pom.nodes()):
            if not "out" in pom.nodes[node]:
                continue
            outs = [b for (a, b) in pom.out_edges(node)]
            found = False
            for node1 in outs:
                if not "in" in pom.nodes[node1]:
                    continue
                if (
                    pom.nodes[node1]["subject"] == pom.nodes[node]["partner"]
                    and pom.nodes[node]["subject"] == pom.nodes[node1]["partner"]
                    and pom.nodes[node1]["in"] == pom.nodes[node]["out"]
                ):
                    found = True
                    break
            if not found:
                last_int += 1
                pom.add_node(
                    last_int, **(dict(pomset.get_matching_label(pom.nodes[node])))
                )
                pom.add_edge(node, last_int)
        return pom

    def gen_cc3(self):
        if self.semantics is None:
            return
        self.delete_folder(self.get_cc3_folder())
        os.makedirs(join(self.get_cc3_folder(), "closure"))
        os.makedirs(join(self.get_cc3_folder(), "synthesis"))

        pomsets = [self.semantics[f] for f in self.semantics]
        self.cc3 = {"closure": {}, "mapping": {}}

        (cc3c, prefixes) = cc3closure(pomsets)
        cc3c = self.filter_cc3_closure(cc3c)
        cc3res = cc3pom(cc3c, prefixes)
        i = 0

        # TODO: remove duplicates after
        for pm in cc3c:
            pm = pomset.transitive_reduction(pm)
            fix_pom_out = self.fix_cc3_counter_example(pm)
            nx.readwrite.graphml.write_graphml(
                fix_pom_out, join(self.get_cc3_folder(), "closure", "%d.graphml" % i)
            )
            utils.debug_pomset(
                fix_pom_out, join(self.get_cc3_folder(), "closure", "%d" % i)
            )
            self.cc3["closure"][i] = fix_pom_out
            if not cc3res[i] is None:
                self.cc3["mapping"][i] = cc3res[i]
            i += 1

    def gen_cc3_choreography(self, pm_idx):
        if self.cc3 is None:
            return
        if not pm_idx in self.cc3["closure"]:
            return
        os.system(
            "pom2gg -d %s %s"
            % (
                join(self.get_cc3_folder(), "synthesis"),
                join(self.get_cc3_folder(), "closure", "%d.graphml" % pm_idx),
            )
        )
        os.system(
            "dot -Tpng %s.dot -o %s"
            % (
                join(self.get_cc3_counter_choreography_folder(pm_idx), "%d" % pm_idx),
                self.get_cc3_counter_choreography_png(pm_idx),
            )
        )
        return os.path.isfile(self.get_cc3_counter_choreography_png(pm_idx))

    def gen_cc3_diff(self, pm_idx, costs):
        if self.cc3 is None:
            return
        if not pm_idx in self.cc3["closure"]:
            return
        pm = self.cc3["closure"][pm_idx]
        g1 = nx.readwrite.graphml.read_graphml(
            self.get_root_folder() + "/choreography.graphml"
        )
        g3 = nx.readwrite.graphml.read_graphml(
            join(
                self.get_cc3_folder(), "synthesis", "%d" % pm_idx, "%d.graphml" % pm_idx
            )
        )
        res = run_diff(g1, g3, self.get_cc3_counter_choreography_folder(pm_idx), costs)
        for i in res:
            os.system(
                "gc2dot -d %s/ --fmt gmldiff %s/diff_%d.graphml"
                % (
                    self.get_cc3_counter_choreography_folder(pm_idx),
                    self.get_cc3_counter_choreography_folder(pm_idx),
                    i,
                )
            )
            os.system(
                "dot -Tpng %s/diff_%d.dot -o %s"
                % (
                    self.get_cc3_counter_choreography_folder(pm_idx),
                    i,
                    self.get_cc3_diff_path(pm_idx, i),
                )
            )
        return res

    def get_termination_folder(self):
        return self.get_root_folder() + "/termination"

    def check_termination(self):
        if self.semantics is None:
            return
        self.delete_folder(self.get_termination_folder())
        os.makedirs(join(self.get_termination_folder()))

        pomsets = [self.semantics[f] for f in self.semantics]
        self.termination = termination.termination_condition(pomsets)
        for p in self.termination:
            for c in self.termination[p]:
                termination.export_termination_counterexample(
                    join(self.get_termination_folder(), p), *c
                )

    def get_projection_folder(self):
        return self.get_root_folder() + "/projections"

    def project(self):
        if self.semantics is None:
            return
        self.delete_folder(self.get_projection_folder())
        os.makedirs(self.get_projection_folder())
        poms = [self.semantics[pomid] for pomid in self.semantics]
        principals = []
        for pom in poms:
            for pr in pomset.get_all_principals(pom):
                if not pr in principals:
                    principals.append(pr)
        self.projections = {}
        for pr in principals:
            self.projections[pr] = proj_to_cfsm(poms, pr)
            export_projection(self.get_projection_folder(), pr, self.projections[pr])
            nx.readwrite.graphml.write_graphml(
                self.projections[pr],
                join(self.get_projection_folder(), "%s.graphml" % pr),
            )


UI_INFO = """
<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <menuitem action='FileOpenChoreography' />
      <menuitem action='FileQuit' />
    </menu>
    <menu action='GenerateMenu'>
      <menuitem action='FileGenSemantics' />
      <menuitem action='Projection' />
      <menuitem action='FileCosts' />
    </menu>
    <menu action='AnalysesMenu'>
      <menuitem action='CC2' />
      <menuitem action='CC3' />
      <menu action='CounterexampleMenu'>
        <menuitem action='pom2sgg' />
        <menuitem action='sgc2diff' />
      </menu>
      <separator />
      <menuitem action='Termination' />
    </menu>
  </menubar>
  <toolbar name='ToolBar'>
    <toolitem action='FileOpenChoreography' />
  </toolbar>
</ui>
"""


class MainWindow(Gtk.Window):
    def __init__(self):
        Gtk.Window.__init__(self)

        self.workspace = None
        # reverse mapping for tree-vew
        self.tree_mapping = {}

        self.set_default_size(800, 600)

        action_group = Gtk.ActionGroup("my_actions")

        self.add_file_menu_actions(action_group)

        uimanager = self.create_ui_manager()
        uimanager.insert_action_group(action_group)

        menubar = uimanager.get_widget("/MenuBar")

        box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        box.pack_start(menubar, False, False, 0)

        toolbar = uimanager.get_widget("/ToolBar")
        box.pack_start(toolbar, False, False, 0)

        self.vp = Gtk.HPaned()
        self.vp.set_position(200)
        box.pack_start(self.vp, True, True, 0)

        # the data are stored in the model
        # create a treestore with two columns
        self.store = Gtk.TreeStore(str, str, str)

        # the treeview shows the model
        # create a treeview on the model self.store
        view = Gtk.TreeView()
        view.set_model(self.store)

        # the cellrenderer for the first column - text
        renderer_books = Gtk.CellRendererText()
        # the first column is created
        main_column = Gtk.TreeViewColumn(None, renderer_books, text=2)
        # and it is appended to the treeview
        view.append_column(main_column)

        self.selection = view.get_selection()
        self.selection.connect("changed", self.on_tree_selection_changed)

        scrolled_window_left = Gtk.ScrolledWindow()
        scrolled_window_left.set_border_width(5)
        scrolled_window_left.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC
        )
        scrolled_window_left.add(view)
        self.vp.add1(scrolled_window_left)

        self.scrolled_window = Gtk.ScrolledWindow()
        self.scrolled_window.set_border_width(5)
        # we scroll only if needed
        self.scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC
        )
        self.vp.add2(self.scrolled_window)

        self.add(box)

    def on_tree_selection_changed(self, selection):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        elif model[treeiter][0] == "root":
            return self.show_choreography_source()
        elif model[treeiter][0] == "choreography-graph":
            return self.show_choreography_graph()

        key = (model[treeiter][0], model[treeiter][1])

        print(key)

        if not key in self.tree_mapping:
            return
        val = self.tree_mapping[key]
        if key[0] == "semantics-pom":
            self.show_pomset_graph(val)
        elif key[0] == "cc2-closure-pom":
            self.show_cc2_closure(val)
        elif key[0] == "cc2-counterexamples-pom":
            self.show_cc2_closure(val)
        elif key[0] == "cc2-counterexamples-sgg":
            self.show_cc2_sgg(val)
        elif key[0] == "cc2-counterexamples-diff":
            self.show_cc2_diff(val)
        elif key[0] == "cc3-closure-pom":
            self.show_cc3_closure(val)
        elif key[0] == "cc3-counterexamples-pom":
            self.show_cc3_closure(val)
        elif key[0] == "cc3-counterexamples-sgg":
            self.show_cc3_sgg(val)
        elif key[0] == "cc3-counterexamples-diff":
            self.show_cc3_diff(val)
        elif key[0] == "termination-counterexamples":
            self.show_termination_counterexample(val)
        elif key[0] == "projection":
            self.show_projection(val)

    def add_file_menu_actions(self, action_group):
        action_filemenu = Gtk.Action("FileMenu", "File", None, None)
        action_group.add_action(action_filemenu)

        # action_filenewmenu = Gtk.Action("FileNew", None, None, Gtk.STOCK_NEW)
        # action_group.add_action(action_filenewmenu)

        # action_new = Gtk.Action("FileNewStandard", "_New",
        #     "Create a new file", Gtk.STOCK_NEW)
        # action_new.connect("activate", self.on_menu_file_new_generic)
        # action_group.add_action_with_accel(action_new, None)

        # action_group.add_actions([
        #     ("FileNewFoo", None, "New Foo", None, "Create new foo",
        #      self.on_menu_file_new_generic),
        #     ("FileNewGoo", None, "_New Goo", None, "Create new goo",
        #      self.on_menu_file_new_generic),
        # ])

        action_filequit = Gtk.Action("FileQuit", None, None, Gtk.STOCK_QUIT)
        action_filequit.connect("activate", self.on_menu_file_quit)
        action_group.add_action_with_accel(action_filequit, "<Control>q")

        action_filecosts = Gtk.Action("FileCosts", "Set Costs", None, None)
        action_filecosts.connect("activate", self.on_menu_file_costs)
        action_group.add_action_with_accel(action_filecosts, "<Control>c")

        action_fileopen = Gtk.Action(
            "FileOpenChoreography", "_Open", "Open .sgg", Gtk.STOCK_OPEN
        )
        action_fileopen.connect("activate", self.on_menu_file_open)
        action_group.add_action_with_accel(action_fileopen)

        action_generatemenu = Gtk.Action("GenerateMenu", "Generate", None, None)
        action_group.add_action(action_generatemenu)

        action_semantics = Gtk.Action(
            "FileGenSemantics", "_Semantics", "Generate Pomset Semantics", None
        )
        action_semantics.connect("activate", self.on_menu_gen_semantics)
        action_group.add_action_with_accel(action_semantics, "<Control>s")

        action_project = Gtk.Action(
            "Projection", "Projection", "Generate Projections", None
        )
        action_project.connect("activate", self.on_menu_project)
        action_group.add_action_with_accel(action_project, "<Control>p")

        action_analysesmenu = Gtk.Action("AnalysesMenu", "Analyses", None, None)
        action_group.add_action(action_analysesmenu)

        action_cc2 = Gtk.Action("CC2", "CC_2", "Closure Condition 2", None)
        action_cc2.connect("activate", self.on_menu_cc2)
        action_group.add_action_with_accel(action_cc2, "<Control>2")

        action_cc3 = Gtk.Action("CC3", "CC_3", "Closure Condition 3", None)
        action_cc3.connect("activate", self.on_menu_cc3)
        action_group.add_action_with_accel(action_cc3, "<Control>3")

        action_counterexamplemenu = Gtk.Action(
            "CounterexampleMenu", "Counterexample", None, None
        )
        action_group.add_action(action_counterexamplemenu)

        action_pom2sgg = Gtk.Action("pom2sgg", "Generate Graph", "Generate Graph", None)
        action_pom2sgg.connect("activate", self.on_menu_pom2sgg)
        action_group.add_action_with_accel(action_pom2sgg, "<Control>g")

        action_sgc2diff = Gtk.Action(
            "sgc2diff", "Compare", "Compare Choreography", None
        )
        action_sgc2diff.connect("activate", self.on_menu_sgc2diff)
        action_group.add_action_with_accel(action_sgc2diff, "<Control>d")

        action_termination = Gtk.Action(
            "Termination", "Termination", "Termination Condition", None
        )
        action_termination.connect("activate", self.on_menu_termination)
        action_group.add_action_with_accel(action_termination, "<Control>t")

    def create_ui_manager(self):
        uimanager = Gtk.UIManager()

        # Throws exception if something went wrong
        uimanager.add_ui_from_string(UI_INFO)

        # Add the accelerator group to the toplevel window
        accelgroup = uimanager.get_accel_group()
        self.add_accel_group(accelgroup)
        return uimanager

    def on_menu_file_new_generic(self, widget):
        print("A File|New menu item was selected.")

    def on_menu_file_open(self, widget):
        dialog = Gtk.FileChooserDialog(
            "Please choose a choreography",
            self,
            Gtk.FileChooserAction.OPEN,
            (
                Gtk.STOCK_CANCEL,
                Gtk.ResponseType.CANCEL,
                Gtk.STOCK_OPEN,
                Gtk.ResponseType.OK,
            ),
        )
        self.add_filters(dialog)
        response = dialog.run()

        if response == Gtk.ResponseType.OK:
            self.workspace = Workspace(dialog.get_filename())
            dialog.destroy()
        else:
            dialog.destroy()
            return

        self.store.clear()
        self.workspace.gen_choreography_png()

        choreography = self.store.append(
            None, ["root", None, self.workspace.get_root_folder()]
        )
        choreography_png = self.store.append(
            choreography, ["choreography-graph", None, "graph"]
        )
        self.tree_mapping = {}

        self.show_choreography_source()

    def remove_tree_root_section(self, section):
        it = self.store.get_iter_first()
        while not it is None:
            if self.store[it][0] == section:
                self.store.remove(it)
                break
            it = self.store.iter_next(it)

    def on_menu_gen_semantics(self, widget):
        self.workspace.gen_semantics()
        self.remove_tree_root_section("semantics")
        semantics = self.store.append(None, ["semantics", None, "semantics"])
        i = 0
        for f in self.workspace.semantics:
            self.store.append(semantics, ["semantics-pom", str(i), "pomset %d" % i])
            self.tree_mapping[("semantics-pom", str(i))] = f
            i += 1

    def closure_res_to_tree(self, closure, res):
        self.remove_tree_root_section("cc%d" % closure)
        cc_list = self.store.append(None, ["cc%d" % closure, None, "CC %d" % closure])
        closure_list = self.store.append(
            cc_list, ["cc%d-closure" % closure, None, "closure"]
        )
        counterexamples_list = self.store.append(
            cc_list, ["cc%d-counterexamples" % closure, None, "counterexamples"]
        )

        for pm in res["closure"]:
            str_view = "pomset %d" % pm
            if pm in res["mapping"]:
                str_view += " -> %d" % res["mapping"][pm]
            self.store.append(
                closure_list, ["cc%d-closure-pom" % closure, str(pm), str_view]
            )
            self.tree_mapping[("cc%d-closure-pom" % closure, str(pm))] = pm
            if not pm in res["mapping"]:
                self.store.append(
                    counterexamples_list,
                    ["cc%d-counterexamples-pom" % closure, str(pm), str_view],
                )
                self.tree_mapping[("cc%d-counterexamples-pom" % closure, str(pm))] = pm

    def on_menu_cc2(self, widget):
        self.workspace.gen_cc2()
        self.closure_res_to_tree(2, self.workspace.cc2)

    def get_selected_cc_pom_idx(self, closure):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return None
        key = (model[treeiter][0], model[treeiter][1])
        if key[0] == ("cc%d-counterexamples-pom" % closure):
            return
        if not key in self.tree_mapping:
            return

    def on_menu_pom2sgg(self, widget):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        key = (model[treeiter][0], model[treeiter][1])
        ccprefix = None
        if key[0] == "cc2-counterexamples-pom":
            ccprefix = "cc2"
        elif key[0] == "cc3-counterexamples-pom":
            ccprefix = "cc3"
        if ccprefix is None:
            return

        pom = self.tree_mapping[key]
        if ccprefix == "cc2":
            res = self.workspace.gen_cc2_choreography(pom)
        else:
            res = self.workspace.gen_cc3_choreography(pom)
        if res:
            self.store.append(
                treeiter, ["%s-counterexamples-sgg" % ccprefix, str(pom), "graph"]
            )
            self.tree_mapping[("%s-counterexamples-sgg" % ccprefix, str(pom))] = pom
            return
        self.store.append(
            treeiter, ["", "", "pomset cannot be represented as global graph"]
        )

    def on_menu_file_costs(self, widget):
        win = CostWindow(self)
        win.show()

    def on_menu_sgc2diff(self, widget):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        key = (model[treeiter][0], model[treeiter][1])

        ccprefix = None
        if key[0] == "cc2-counterexamples-sgg":
            ccprefix = "cc2"
        elif key[0] == "cc3-counterexamples-sgg":
            ccprefix = "cc3"
        if ccprefix is None:
            return

        if not key in self.tree_mapping:
            return

        win = CostWindow(self)
        win.show()

    def diff_exec(self, costs):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        key = (model[treeiter][0], model[treeiter][1])

        ccprefix = None
        if key[0] == "cc2-counterexamples-sgg":
            ccprefix = "cc2"
        elif key[0] == "cc3-counterexamples-sgg":
            ccprefix = "cc3"
        if ccprefix is None:
            return

        if not key in self.tree_mapping:
            return

        pom = self.tree_mapping[key]

        if ccprefix == "cc2":
            diffs = self.workspace.gen_cc2_diff(pom, costs)
        else:
            diffs = self.workspace.gen_cc3_diff(pom, costs)

        diff_iter = self.store.append(
            treeiter, ["%s-counterexamples-diff-list" % ccprefix, None, "diffs"]
        )
        for i in diffs:
            new_iter = self.store.append(
                diff_iter,
                [
                    "%s-counterexamples-diff" % ccprefix,
                    "%d-%d" % (pom, i),
                    "%d: %f" % (i, diffs[i]),
                ],
            )
            self.tree_mapping[
                ("%s-counterexamples-diff" % ccprefix, "%d-%d" % (pom, i))
            ] = (pom, i)

    def on_menu_cc3(self, widget):
        self.workspace.gen_cc3()
        self.closure_res_to_tree(3, self.workspace.cc3)

    def on_menu_termination(self, widget):
        self.workspace.check_termination()
        self.remove_tree_root_section("termination")
        term_list = self.store.append(
            None, ["termination", None, "Termination counterexamples"]
        )

        error_num = 0
        for p in self.workspace.termination:
            p_list = self.store.append(term_list, ["termination-principal", p, p])
            print(self.workspace.termination[p])
            for (
                id_pom1,
                id_pom2,
                pom1,
                pom2,
                mapping,
                mins,
            ) in self.workspace.termination[p]:
                str_view = "pomset %d -> %d" % (id_pom1, id_pom2)
                self.store.append(
                    p_list, ["termination-counterexamples", str(error_num), str_view]
                )
                self.tree_mapping[("termination-counterexamples", str(error_num))] = (
                    p,
                    id_pom1,
                    id_pom2,
                )
                error_num += 1

    def on_menu_project(self, widget):
        self.workspace.project()
        self.remove_tree_root_section("projection")
        proj_list = self.store.append(None, ["projection", None, "Projections"])

        for p in self.workspace.projections:
            p_list = self.store.append(proj_list, ["projection", p, p])
            self.tree_mapping[("projection", p)] = p

    def change_main_view(self, widget):
        old_views = self.scrolled_window.get_children()
        for old in old_views:
            self.scrolled_window.remove(old)
        self.scrolled_window.add(widget)
        self.scrolled_window.show_all()

    def show_choreography_source(self):
        self.change_main_view(Gtk.Label(open(self.workspace.sgg_absolute_path).read()))

    def show_choreography_graph(self):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_choreography_png_path())
        )

    def show_pomset_graph(self, f):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_semantics_png_path(f))
        )

    def show_cc2_closure(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_cc2_closure_png_path(i))
        )

    def show_cc2_sgg(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_cc2_counter_choreography_png(i))
        )

    def show_cc2_diff(self, val):
        (pom_id, i) = val
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_cc2_diff_path(pom_id, i))
        )

    def show_cc3_closure(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_cc3_closure_png_path(i))
        )

    def show_cc3_sgg(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_cc3_counter_choreography_png(i))
        )

    def show_cc3_diff(self, val):
        (pom_id, i) = val
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.get_cc3_diff_path(pom_id, i))
        )

    def show_termination_counterexample(self, val):
        (p, id_pom1, id_pom2) = val
        self.change_main_view(
            Gtk.Image.new_from_file(
                "%s/%d-%d.png"
                % (join(self.workspace.get_termination_folder(), p), id_pom1, id_pom2)
            )
        )

    def show_projection(self, val):
        self.change_main_view(
            Gtk.Image.new_from_file(
                join(self.workspace.get_projection_folder(), "%s.png" % val)
            )
        )

    def add_filters(self, dialog):
        filter_sgg = Gtk.FileFilter()
        filter_sgg.set_name("sgg files")
        filter_sgg.add_pattern("*.sgg")
        dialog.add_filter(filter_sgg)

        filter_any = Gtk.FileFilter()
        filter_any.set_name("Any files")
        filter_any.add_pattern("*")
        dialog.add_filter(filter_any)

    def on_menu_file_quit(self, widget):
        Gtk.main_quit()


class CostWindow(Gtk.Window):
    def __init__(self, main_window):
        Gtk.Window.__init__(self, title="Edit Distance costs")
        self.main_window = main_window
        self.set_default_size(200, 200)
        table = Gtk.Table(4, 6, True)

        self.mapval = {
            "delete_node": {"value": 0.45, "txt": "Delete node", "pos": (0, 0)},
            "insert_node": {"value": 0.45, "txt": "Insert node", "pos": (1, 0)},
            "change_open_gate": {
                "value": 0.1,
                "txt": "Change open gate",
                "pos": (0, 1),
            },
            "change_close_gate": {
                "value": 0.1,
                "txt": "Change close gate",
                "pos": (1, 1),
            },
            "change_sender": {"value": 0.2, "txt": "Change sender", "pos": (0, 2)},
            "change_receiver": {"value": 0.2, "txt": "Change receiver", "pos": (1, 2)},
            "change_payload": {"value": 0.1, "txt": "Change payload", "pos": (0, 3)},
            "delete_edge": {"value": 0.2, "txt": "Delete edge", "pos": (0, 4)},
            "insert_edge": {"value": 0.2, "txt": "Insert edge", "pos": (1, 4)},
        }
        for k in self.mapval:
            self.mapval[k]["label"] = Gtk.Label(self.mapval[k]["txt"])
            self.mapval[k]["entry"] = Gtk.Entry()
            self.mapval[k]["entry"].set_text("%.2f" % self.mapval[k]["value"])

        def attach_elem(name, x, y):
            table.attach(self.mapval[name]["label"], 2 * x, 2 * x + 1, y, y + 1)
            table.attach(self.mapval[name]["entry"], 2 * x + 1, 2 * x + 2, y, y + 1)

        for k in self.mapval:
            attach_elem(k, self.mapval[k]["pos"][0], self.mapval[k]["pos"][1])

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        table.attach(button, 0, 1, 5, 6)

        button = Gtk.Button.new_with_mnemonic("_Execute")
        button.connect("clicked", self.on_execute_clicked)
        table.attach(button, 1, 2, 5, 6)

        self.add(table)
        self.show_all()

    def on_close_clicked(self, widget):
        self.close()

    def on_execute_clicked(self, widget):
        res = {}
        for k in self.mapval:
            res[k] = float(self.mapval[k]["entry"].get_text())
        self.main_window.diff_exec(res)
        self.close()


if __name__ == "__main__":
    win = MainWindow()
    win.connect("destroy", Gtk.main_quit)
    win.show_all()
    Gtk.main()

