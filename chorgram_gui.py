from tkinter import *


def mk_menu(key, theMenu):
    (name, actions) = theMenu
    name.add_cascade(label=key, menu=name)
    for (cmdDisplay, cmd) in actions:
        name.add_command(label=cmdDisplay, command=cmd)

        
def newFile():
    print("New File!")
def openFile():
    name = askopenfilename()
    print(name)
def saveFile():
    name = askopenfilename()
    print(name)
def saveAs():
    name = askopenfilename()
    print(name)
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

menus = {}

root = Tk()

# w = Label(root)

menubar = Menu(root)

# Defining menus
filemenu = Menu(menubar, tearoff = 0)
menus["File"] = (filemenu, [("New File", newFile),
                            ("Open File", openFile),
                            ("Save File", saveFile),
                            ("Save As", saveAs)]
)
mk_menu("File", menus["File"])

toolsmenu = Menu(menubar)
menus["Tools"] = (toolsmenu, [("gg2cfsm", gg2cfsm),
                              ("gg2erl", gg2erl),
                              ("cfsm2gg", cfsm2gg),
                              ("check GMC", checkGMC),
                              ("Parse system of CFSMs", sysparser),
                              ("semantics", semantics)]
)
configmenu = Menu(menubar)
menus["Config"] = (configmenu,[("DOT", dummyCmd),
                               ("X", dummyCmd),
                               ("Y", dummyCmd),
                               ("Z", dummyCmd)]
)
helpmenu = Menu(menubar)
menus["Help"] = (helpmenu, [("About...", about),
                            ("Help", dummyCmd)]
)


# for k in menus.keys():
#     print(k)
#     mk_menu(k, menus[k])


root.config(menu = menubar)

root.mainloop()
