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
from ccpom import *

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

        self.choography_file_path = None

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
        self.pomset_mapping = {}
        self.cc2_closure = {}

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

        label1 = Gtk.Label("Right-click to see the popup menu.")
        self.vp.add2(label1)

        self.add(box)

    def on_tree_selection_changed(self, selection):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        print("You selected", model[treeiter][0])
        if model[treeiter][0] == "choreography":
            self.show_choreography_source()
            return
        if model[treeiter][0] == "choreography-graph":
            self.show_choreography_graph()
            return
        if model[treeiter][0] in self.pomset_mapping:
            self.show_pomset_graph(self.pomset_mapping[model[treeiter][0]])
        if model[treeiter][0] in self.cc2_closure:
            self.show_cc2_closure(self.cc2_closure[model[treeiter][0]])

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
        action_group.add_action(action_fileopen)

        action_semantics = Gtk.Action("FileGenSemantics", "Generate _Semantics", "Generate Pomset Semantics", None)
        action_semantics.connect("activate", self.on_menu_gen_semantics)
        action_group.add_action(action_semantics)

        action_cc2 = Gtk.Action("CC2", "CC_2", "Closure Condition 2", None)
        action_cc2.connect("activate", self.on_menu_cc2)
        action_group.add_action(action_cc2)

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
        print("A File|Open file.")
        dialog = Gtk.FileChooserDialog("Please choose a file", self,
            Gtk.FileChooserAction.OPEN,
            (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
             Gtk.STOCK_OPEN, Gtk.ResponseType.OK))

        self.add_filters(dialog)

        response = dialog.run()
        if response == Gtk.ResponseType.OK:
            print("Open clicked")
            self.choography_file_path = dialog.get_filename()
            dialog.destroy()
        elif response == Gtk.ResponseType.CANCEL:
            print("Cancel clicked")
            dialog.destroy()
            return

        self.store.clear()
        choreography = self.store.append(None, ["choreography", os.path.basename(self.choography_file_path)])
        choreography_png = self.store.append(choreography,
                                             ["choreography-graph", "graph"])


        os.system("../gg2gml %s > %s" % (
            self.choography_file_path,
            os.path.dirname(self.choography_file_path) + "/choreography.graphml"
        ))
        # this is already a png
        os.system("../chor2dot -d %s -fmt sloppygml %s" % (
            os.path.dirname(self.choography_file_path) + "/", 
            os.path.dirname(self.choography_file_path) + "/choreography.graphml"
        ))

        os.system('dot -Tpng %s -o %s' % (
            os.path.dirname(self.choography_file_path) + "/choreography.dot",
            os.path.dirname(self.choography_file_path) + "/choreography.png"))

        self.show_choreography_source()

    def list_files_in_folder(self, folder):
        files = [f for f in listdir(folder) if isfile(join(folder, f))]
        return files

    def get_semantics_folder(self):
        return self.choography_file_path.split(".")[0]

    def on_menu_gen_semantics(self, widget):
        # I should ensure this goes in the third position and remove all the past semantics from the tree
        shutil.rmtree(self.get_semantics_folder())
        cmd = "../gg2pom -d %s --gml %s" % (
            os.path.dirname(self.choography_file_path) + "/", 
            self.choography_file_path
        )
        os.system(cmd)
        semantics = self.store.append(None, ["semantics", "semantics"])
        i = 4
        self.pom_semantics = []
        for f in self.list_files_in_folder(self.get_semantics_folder()):
            store_name = "semantics-%d"%i
            self.store.append(semantics, [store_name, f])
            graph = nx.readwrite.graphml.read_graphml(join(self.get_semantics_folder(), f))
            utils.debug_pomset(graph, join(self.get_semantics_folder(), f))
            self.pomset_mapping[store_name] = f
            self.pom_semantics.append(graph)
            i+=1
        
    def on_menu_cc2(self, widget):
        cc2c = cc2closure(self.pom_semantics)
        # cc2res = cc2pom(cc2c, global_view)
        # cc2err = counterexamples(cc2c, cc2res)
        analysis_list = self.store.append(None, ["analysis", "analysis"])
        cc2_list = self.store.append(analysis_list, ["cc2", "CC 2"])
        closure_list = self.store.append(cc2_list, ["cc2-closure", "closure"])
        i = 0
        self.cc2_closure = {}
        for pomset in cc2c:
            store_name = "cc2-closure-%d"%i
            self.store.append(closure_list, [store_name, "pomset%d"%i])
            utils.debug_pomset(pomset, join(os.path.dirname(self.choography_file_path) + "/cc2/closure/", "%d"%i))
            self.cc2_closure[store_name] = i
            i+=1
    
    def show_choreography_source(self):
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_border_width(5)
        # we scroll only if needed
        scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        label = Gtk.Label(open(self.choography_file_path).read())
        scrolled_window.add(label)
        self.vp.remove(self.vp.get_child2());
        self.vp.add2(scrolled_window)
        scrolled_window.show_all()
        
    def show_choreography_graph(self):
        img = Gtk.Image.new_from_file(
            os.path.dirname(self.choography_file_path) + "/choreography.png")
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_border_width(5)
        # we scroll only if needed
        scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled_window.add(img)
        self.vp.remove(self.vp.get_child2());
        self.vp.add2(scrolled_window)
        scrolled_window.show_all()

    def show_pomset_graph(self, f):
        img = Gtk.Image.new_from_file(
            self.get_semantics_folder() + "/%s.png" % f)
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_border_width(5)
        # we scroll only if needed
        scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled_window.add(img)
        self.vp.remove(self.vp.get_child2());
        self.vp.add2(scrolled_window)
        scrolled_window.show_all()

    def show_cc2_closure(self, i):
        img = Gtk.Image.new_from_file(
            join(os.path.dirname(self.choography_file_path) + "/cc2/closure/", "%d.png"%i))
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_border_width(5)
        # we scroll only if needed
        scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled_window.add(img)
        self.vp.remove(self.vp.get_child2());
        self.vp.add2(scrolled_window)
        scrolled_window.show_all()

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
