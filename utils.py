# Author: Emilio Tuosto <emilio.tuosto@gssi.it>
#
# Some utilities for python scripts

import os
import os.path

RND_SIZE = 100
RND_PTPS = 20
RND_MSGS = 20

def debugMsg(debug, cmd, msg, force = False):
    """Prints debugging messages"""
    if (debug or force):
        s1 = ": " + ("{--- " if not(force) else "")
        s2 = (" ---}" if not(force) else "")
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
