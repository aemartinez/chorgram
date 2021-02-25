#!/usr/bin/python3


# Author: Emilio Tuosto <emilio@le.ac.uk>
#
# My first GUI :)
# I used https://tkdocs.com/tutorial/

from tkinter import *
from tkinter import ttk
from tkinter.filedialog import *

menu = {}
root = Tk(className="ChorGram")
# mainframe = ttk.Frame(root, padding="3 3 12 12")
# mainframe.grid(column=0, row=0, sticky=(N, W, E, S))
# root.columnconfigure(0, weight=1)
# root.rowconfigure(0, weight=1)
menubar = Menu(root)
root.config(menu = menubar)

def retrieve_input(textBox):
    text = textBox.get("1.0",END)
    print(text)
    return text

def newFile():
    textBox=Text(root, height=20, width=100)
    textBox.pack()
    buttonCommit=Button(root, height=1, width=10, text="Commit", 
                        command=lambda: retrieve_input(textBox))
    buttonCommit.pack()

def openFile():
    name = askopenfilename()
    print("open " + name)
def saveFile():
    name = askopenfilename()
    print("save " + name)
def saveAs():
    name = askopenfilename()
    print("save as " + name)
def about():
    print("Something about ChorGram")

def dummyCmd():
    print("I am a dummy coommand")

def gg2cfsm():
    dummyCmd()

def gg2erl():
    dummyCmd()

def cfsm2gg():
    dummyCmd()

def semantics():
    dummyCmd()

def checkGMC():
    dummyCmd()

def sysparser():
    dummyCmd()

def mk_menu(key, name_actions):
    menubar.add_cascade(label=key, menu=name_actions[0])
    for t in name_actions[1]:
        if len(t) < 3:
            name_actions[0].add_command(label=t[0], command=t[1])
        else:
            name_actions[0].add_command(label=t[0], command=t[1], accelerator=t[2])


# Defining menus
filemenu = Menu(menubar, tearoff = 0)
menu["File"] = (filemenu, [("New File", newFile, "Ctrl+n"),
                           ("Open File", openFile, "Ctrl+o"),
                           ("Save File", saveFile, "Ctrl+s"),
                           ("Save As", saveAs, "Ctrl+Shift+s"),
                           ("Exit", root.quit, "Ctrl+q")]
)

toolsmenu = Menu(menubar)
menu["Tools"] = (toolsmenu, [("gg2cfsm", gg2cfsm),
                              ("gg2erl", gg2erl),
                              ("cfsm2gg", cfsm2gg),
                              ("check GMC", checkGMC),
                              ("Parse system of CFSMs", sysparser),
                              ("semantics", semantics)]
)

configmenu = Menu(menubar)
menu["Config"] = (configmenu,[("Graphic output", dummyCmd),
                               ("Auxiliary tools", dummyCmd),
                               ("Y", dummyCmd),
                               ("Z", dummyCmd)]
)

helpmenu = Menu(menubar)
menu["Help"] = (helpmenu, [("About...", about),
                            ("Help", dummyCmd)]
)

for k in ["File", "Tools", "Config", "Help"]:
    mk_menu(k, menu[k])

root.mainloop()
