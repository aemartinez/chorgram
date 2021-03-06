# Author: Emilio Tuosto <emilio.tuosto@gssi.it>
#
# This python script combines the parts of the tool

import os
import os.path

def debugMsg(debug, cmd, msg, force = False):
    """Prints debugging messages"""
    if (debug or force):
        s1 = (": {---" if not(force) else ": ")
        s2 = ("---}" if not(force) else "")
        print(cmd + s1 + msg + s2)
        
def mkdir(dir):
    """Creates a directory, if needed"""
    try:
        os.makedirs(dir)
    except OSError:
        if os.path.exists(dir):
            pass
        else:
            raise
