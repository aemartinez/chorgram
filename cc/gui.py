#!/usr/bin/python3

import os
from os import listdir
from os.path import isfile, join
import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, Gdk
import subprocess
import networkx as nx
import utils
import shutil
import pomset
from ccpom import *
from diff import run_diff

class Workspace():
    def __init__(self, sgg_path):
        self.sgg_absolute_path = sgg_path
        self.semantics = None
        self.cc2 = None
        self.cc3 = None
        self.projections = None

    def get_root_folder(self):
        return os.path.dirname(self.sgg_absolute_path)
    
    def gen_choreography_graphml(self):
        os.system("../gg2gml %s > %s" % (
            self.sgg_absolute_path,
            self.get_root_folder() + "/choreography.graphml"
        ))

    def get_choreography_png_path(self):
        return self.get_root_folder() + "/choreography.png"
    
    def gen_choreography_png(self):
        self.gen_choreography_graphml()
        os.system("../chor2dot -d %s/ -fmt sloppygml %s/choreography.graphml" % (
            self.get_root_folder(), 
            self.get_root_folder()
        ))
        os.system('dot -Tpng %s/choreography.dot -o %s' % (
            self.get_root_folder(),
            self.get_choreography_png_path())
        )

    def get_semantics_folder(self):
        return self.sgg_absolute_path.split(".")[0]

    def list_files_in_folder(self, folder):
        files = [f for f in listdir(folder) if isfile(join(folder, f))]
        return files

    def get_semantics_png_path(self, f):
        return self.get_semantics_folder() + "/%s.png" % f


    def delete_folder(self, folder):
        try:
            shutil.rmtree(folder)
        except:
            pass
    
    def gen_semantics(self):
        # I should ensure this goes in the third position and remove all the past semantics from the tree
        self.delete_folder(self.get_semantics_folder())
        cmd = "../gg2pom -d %s/ --gml %s" % (
            self.get_root_folder(), 
            self.sgg_absolute_path
        )
        os.system(cmd)
        self.semantics = {}
        for f in self.list_files_in_folder(self.get_semantics_folder()):
            graph = nx.readwrite.graphml.read_graphml(join(self.get_semantics_folder(), f))
            self.semantics[f] = graph
            utils.debug_pomset(graph, join(self.get_semantics_folder(), f))

    def get_cc2_folder(self):
        return self.get_root_folder() + "/cc2"
    
    def gen_cc2(self):
        if self.semantics is None:
            return
        self.delete_folder(self.get_cc2_folder())
        os.makedirs(join(self.get_cc2_folder(),  "closure"))
        os.makedirs(join(self.get_cc2_folder(),  "synthesis"))
        
        pomsets = [self.semantics[f] for f in self.semantics]
        cc2c = cc2closure(pomsets)
        self.cc2 = {"closure": {}, "mapping": {}}
        cc2res = cc2pom(cc2c, pomsets)
        i = 0
        for pm in cc2c:
            # TODO: we should use the transitive reduction, but it does not work
            nx.readwrite.graphml.write_graphml(pm, join(self.get_cc2_folder(),  "closure", "%d.graphml"%i))
            utils.debug_pomset(pm, join(self.get_cc2_folder(),  "closure", "%d"%i))
            self.cc2["closure"][i] = pm
            if not cc2res[i] is None:
                self.cc2["mapping"][i] = cc2res[i]
            i+=1

    def get_cc2_closure_png_path(self, i):
        return join(self.get_cc2_folder(),  "closure", "%d.png"%i)

    def get_cc2_counter_choreography_folder(self, pm_idx):
        return join(self.get_cc2_folder(),  "synthesis", "%d"%pm_idx)
    def get_cc2_counter_choreography_png(self, pm_idx):
        return join(self.get_cc2_counter_choreography_folder(pm_idx), "%d.png"%pm_idx)
    
    def gen_cc2_choreography(self, pm_idx):
        if self.cc2 is None:
            return
        if not pm_idx in self.cc2["closure"]:
            return
        pm = self.cc2["closure"][pm_idx]
        os.system('../pom2gg -d %s %s' % (
            join(self.get_cc2_folder(),  "synthesis"),
            join(self.get_cc2_folder(),  "closure", "%d.graphml"%pm_idx)
        ))
        os.system('dot -Tpng %s.dot -o %s' % (
            join(self.get_cc2_counter_choreography_folder(pm_idx), "%d"%pm_idx),
            self.get_cc2_counter_choreography_png(pm_idx))
        )
    def gen_cc2_diff(self, pm_idx):
        if self.cc2 is None:
            return
        if not pm_idx in self.cc2["closure"]:
            return
        pm = self.cc2["closure"][pm_idx]
        g1 = nx.readwrite.graphml.read_graphml(self.get_root_folder() + "/choreography.graphml")
        g2 = nx.readwrite.graphml.read_graphml(join(self.get_cc2_folder(),  "closure", "%d.graphml"%pm_idx))
        res = run_diff(g1, g2, self.get_cc2_counter_choreography_folder(pm_idx))
        for i in res:
            os.system("../chor2dot -d %s/ -fmt sloppygml %s/diff_%d.graphml" % (
                self.get_cc2_counter_choreography_folder(pm_idx), 
                self.get_cc2_counter_choreography_folder(pm_idx),
                i
            ))
            os.system('dot -Tpng %s/diff_%d.dot -o %s/diff_%d.png' % (
                self.get_cc2_counter_choreography_folder(pm_idx),
                i,
                self.get_cc2_counter_choreography_folder(pm_idx),
                i
            ))
            
        

UI_INFO = """
<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <menuitem action='FileOpenChoreography' />
      <menu action='FileNew'>
        <menuitem action='FileNewStandard' />
        <menuitem action='FileNewFoo' />
        <menuitem action='FileNewGoo' />
      </menu>
      <separator />
      <menuitem action='FileGenSemantics' />
      <separator />
      <menuitem action='CC2' />
      <menuitem action='pom2sgg' />
      <menuitem action='sgg2diff' />
      <separator />
      <menuitem action='FileQuit' />
    </menu>
  </menubar>
  <toolbar name='ToolBar'>
    <toolitem action='FileOpenChoreography' />
    <toolitem action='FileQuit' />
  </toolbar>
</ui>
"""




class MainWindow(Gtk.Window):

    def __init__(self):
        Gtk.Window.__init__(self, title="PomChor 0.99 Beta 2")

        self.workspace = None
        # reverse mapping for tree-vew
        self.tree_semantics_mapping = {}
        self.tree_cc2_closure_mapping = {}
        self.tree_cc2_counterexamples_mapping = {}
        
        self.set_default_size(600, 400)

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
        box.pack_start(self.vp, True, True, 0)

        # the data are stored in the model
        # create a treestore with two columns
        self.store = Gtk.TreeStore(str, str)
        
        # the treeview shows the model
        # create a treeview on the model self.store
        view = Gtk.TreeView()
        view.set_model(self.store)

        # the cellrenderer for the first column - text
        renderer_books = Gtk.CellRendererText()
        # the first column is created
        main_column = Gtk.TreeViewColumn(None, renderer_books, text=1)
        # and it is appended to the treeview
        view.append_column(main_column)

        self.selection = view.get_selection()
        self.selection.connect("changed", self.on_tree_selection_changed)

        
        # add the treeview to the window
        self.vp.add1(view)

        self.scrolled_window = Gtk.ScrolledWindow()
        self.scrolled_window.set_border_width(5)
        # we scroll only if needed
        self.scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        self.vp.add2(self.scrolled_window)

        self.add(box)

    def on_tree_selection_changed(self, selection):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            pass
        elif model[treeiter][0] == "root":
            self.show_choreography_source()
        elif model[treeiter][0] == "choreography-graph":
            self.show_choreography_graph()
        elif model[treeiter][0] in self.tree_semantics_mapping:
            self.show_pomset_graph(self.tree_semantics_mapping[model[treeiter][0]])
        elif model[treeiter][0] in self.tree_cc2_closure_mapping:
            self.show_cc2_closure(self.tree_cc2_closure_mapping[model[treeiter][0]])
        elif model[treeiter][0] in self.tree_cc2_counterexamples_mapping:
            self.show_cc2_closure(self.tree_cc2_counterexamples_mapping[model[treeiter][0]])

    def add_file_menu_actions(self, action_group):
        action_filemenu = Gtk.Action("FileMenu", "File", None, None)
        action_group.add_action(action_filemenu)

        action_filenewmenu = Gtk.Action("FileNew", None, None, Gtk.STOCK_NEW)
        action_group.add_action(action_filenewmenu)

        action_new = Gtk.Action("FileNewStandard", "_New",
            "Create a new file", Gtk.STOCK_NEW)
        action_new.connect("activate", self.on_menu_file_new_generic)
        action_group.add_action_with_accel(action_new, None)

        action_group.add_actions([
            ("FileNewFoo", None, "New Foo", None, "Create new foo",
             self.on_menu_file_new_generic),
            ("FileNewGoo", None, "_New Goo", None, "Create new goo",
             self.on_menu_file_new_generic),
        ])

        action_filequit = Gtk.Action("FileQuit", None, None, Gtk.STOCK_QUIT)
        action_filequit.connect("activate", self.on_menu_file_quit)
        action_group.add_action(action_filequit)

        action_fileopen = Gtk.Action("FileOpenChoreography", "_Open", "Open .sgg", Gtk.STOCK_OPEN)
        action_fileopen.connect("activate", self.on_menu_file_open)
        action_group.add_action_with_accel(action_fileopen)

        action_semantics = Gtk.Action("FileGenSemantics", "Generate _Semantics", "Generate Pomset Semantics", None)
        action_semantics.connect("activate", self.on_menu_gen_semantics)
        action_group.add_action_with_accel(action_semantics, "<Control>s")

        action_cc2 = Gtk.Action("CC2", "CC_2", "Closure Condition 2", None)
        action_cc2.connect("activate", self.on_menu_cc2)
        action_group.add_action_with_accel(action_cc2, "<Control>2")

        action_pom2sgg = Gtk.Action("pom2sgg", "Generate Choreography", "Generate Choreography", None)
        action_pom2sgg.connect("activate", self.on_menu_pom2sgg)
        action_group.add_action_with_accel(action_pom2sgg, "<Control>p")

        action_sgg2diff = Gtk.Action("sgg2diff", "Compare", "Compare Choreography", None)
        action_sgg2diff.connect("activate", self.on_menu_sgg2diff)
        action_group.add_action_with_accel(action_sgg2diff, "<Control>d")
        
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
        dialog = Gtk.FileChooserDialog("Please choose a choreography", self,
            Gtk.FileChooserAction.OPEN,
            (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
             Gtk.STOCK_OPEN, Gtk.ResponseType.OK))
        self.add_filters(dialog)
        response = dialog.run()
        
        if response == Gtk.ResponseType.OK:
            self.workspace = Workspace(dialog.get_filename())
            dialog.destroy()
        elif response == Gtk.ResponseType.CANCEL:
            dialog.destroy()
            return

        self.store.clear()
        self.workspace.gen_choreography_png()
        
        choreography = self.store.append(
            None,
            ["root", self.workspace.get_root_folder()])
        choreography_png = self.store.append(
            choreography,
            ["choreography-graph", "graph"])

        self.show_choreography_source()

    def remove_tree_root_section(self, section):
        it = self.store.get_iter_first()
        while (not it is None):
            if self.store[it][0] == section:
                self.store.remove(it)
                break
            it = self.store.iter_next(it)
        

    def on_menu_gen_semantics(self, widget):
        self.workspace.gen_semantics()
        self.remove_tree_root_section("semantics")
        semantics = self.store.append(None, ["semantics", "semantics"])
        self.tree_semantics_mapping = {}
        i = 0
        for f in self.workspace.semantics:
            store_name = "semantics-%d"%i
            self.store.append(semantics, [store_name, f])
            self.tree_semantics_mapping[store_name] = f
            i+=1
        
    def on_menu_cc2(self, widget):
        self.workspace.gen_cc2()
        self.remove_tree_root_section("cc2")
        cc2_list = self.store.append(None, ["cc2", "CC 2"])
        closure_list = self.store.append(cc2_list, ["cc2-closure", "closure"])
        counterexamples_list = self.store.append(cc2_list, ["cc2-counterexamples", "counterexamples"])

        self.tree_cc2_closure_mapping = {}
        self.tree_cc2_counterexamples_mapping = {}
        
        for pm in self.workspace.cc2["closure"]:
            store_name = "cc2-closure-%d"%pm
            str_view = "pomset%d"%pm
            if pm in self.workspace.cc2["mapping"]:
                str_view += " -> %d" % self.workspace.cc2["mapping"][pm]
            self.store.append(closure_list, [store_name, str_view])
            self.tree_cc2_closure_mapping[store_name] = pm
            if not pm in self.workspace.cc2["mapping"]:
                store_name = "cc2-counterexample-%d"%pm
                self.store.append(counterexamples_list, [store_name, str_view])
                self.tree_cc2_counterexamples_mapping[store_name] = pm

    def on_menu_pom2sgg(self, widget):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        if not model[treeiter][0] in self.tree_cc2_counterexamples_mapping:
            return

        pom = self.tree_cc2_counterexamples_mapping[model[treeiter][0]]
        self.workspace.gen_cc2_choreography(
            pom
        )

        self.store.append(treeiter, ["cc2-counterexample-graph-%d"%pom, "graph"])
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_cc2_counter_choreography_png(pom)
            ))
        return
                
    def on_menu_sgg2diff(self, widget):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        if not model[treeiter][0] in self.tree_cc2_counterexamples_mapping:
            return

        pom = self.tree_cc2_counterexamples_mapping[model[treeiter][0]]
        self.workspace.gen_cc2_diff(
            pom
        )

    def change_main_view(self, widget):
        old_views = self.scrolled_window.get_children()
        for old in old_views:
            self.scrolled_window.remove(old)
        self.scrolled_window.add(widget)
        self.scrolled_window.show_all()
        
    def show_choreography_source(self):
        self.change_main_view(
            Gtk.Label(open(self.workspace.sgg_absolute_path).read())
        )
        
    def show_choreography_graph(self):
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_choreography_png_path()
            ))


    def show_pomset_graph(self, f):
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_semantics_png_path(f)
            ))

    def show_cc2_closure(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_cc2_closure_png_path(i)
            ))

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



win = MainWindow()
win.connect("destroy", Gtk.main_quit)
win.show_all()
Gtk.main()
