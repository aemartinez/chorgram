from tkinter import *


def mk_menu(m, theMenu):
    (name, actions) = theMenu
    menu.add_cascade(label=m, menu=name)
    for (cmdDisplay, cmd) in actions:
        filemenu.add_command(label=cmdDisplay, command=cmd)

        
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

root = Tk()
menu = Menu(root)

w = Label(root)

root.config(menu=menu)

# Defining menus
filemenu = Menu(menu)
configmenu = Menu(menu)
helpmenu = Menu(menu)

menus = {
    "File" : (filemenu, [("New File", newFile),
                         ("Open File", openFile),
                         ("Save File", saveFile),
                         ("Save As", saveAs)]
    ),
    "Config" : (configmenu,[("DOT", dummyCmd),
                            ("X", dummyCmd),
                            ("Y", dummyCmd),
                            ("Z", dummyCmd)]
    ),
    "Help" : (helpmenu, [("About...", about),
                         ("Help", dummyCmd)]
    )
}


for k in menus.keys():
    mk_menu(k, menus[k])


root.mainloop()
