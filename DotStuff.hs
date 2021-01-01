--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module contains function to map GC and HG to DOT
--

module DotStuff where

import Data.Map.Strict as M
import Misc
import System.Environment (getExecutablePath)
import System.FilePath.Posix

type DotNode = Int

getDotConf :: IO(Map String DotString)
getDotConf = do
  exe <- getExecutablePath
  let cwd = takeDirectory exe
  cfg <- getConf (cwd </> "aux/chorgram.config")
  getConf (cwd </> cfg!"dot")

cpV :: DotString
cpV = " [label=\"\", shape=point, width=0.15, height=0.15, color=darkorange, fillcolor=darkorange, style=filled]\n"

heV :: DotString
heV = " [label=\"\", shape=square, width=0.05, height=0.05, color=blue, fillcolor=whitesmoke, style=filled]\n"

forkV :: DotString
forkV = " [label=\"|\", shape=square, fixedsize=true, fillcolor=papayawhip, style=filled, fontsize=10, fontcolor=sienna]\n"

joinV :: DotString
joinV = " [label=\"|\", shape=square, fixedsize=true, fillcolor=sienna, style=filled, fontsize=10, fontcolor=papayawhip]\n"

branchV :: DotString
branchV = " [label=\"+\", shape=diamond, fixedsize=true, fillcolor=papayawhip, style=filled, fontsize=10, fontcolor=sienna]\n"

mergeV :: DotString
mergeV = " [label=\"+\", shape=diamond, fixedsize=true, fillcolor=sienna, style=filled, fontsize=10, fontcolor=papayawhip]\n"

sourceV :: DotString
sourceV = " [label=\"\", shape=circle, fixedsize=true]\n"

sinkV :: DotString
sinkV = " [label=\"\", shape=circle, width=0.15, height=0.15, fixedsize=true, peripheries=2]\n"

sizeNode :: DotString
sizeNode = "0.2"

qsep :: String
qsep  = "qsep"

bsep :: String
bsep = "bsep"

q0style :: String
q0style = "q0style"

q0col :: String
q0col = "q0col"

statesep :: String
statesep = "statesep"

ptpsep :: String
ptpsep = "ptpsep"

confsep :: String
confsep = "confsep"

tslab :: String
tslab = "tslab"

deadlockcol1 :: String
deadlockcol1 = "deadlockcol1"

deadlockcol2 :: String
deadlockcol2 = "deadlockcol2"

propfgcol :: String
propfgcol = "propfgcol"

propbgcol :: String
propbgcol = "propbgcol"

actioncol :: String
actioncol = "actioncol"

conffgcol :: String
conffgcol = "conffgcol"

confbgcol :: String
confbgcol = "confbgcol"

pathcol :: String
pathcol = "pathcol"

initshape :: String
initshape = "initshape"

initwidth :: String
initwidth = "initwidth"

initcol :: String
initcol = "initcol"

gcnodesep :: String
gcnodesep = "gcnodesep"

nodefont :: String
nodefont = "nodefont"

edgefont :: String
edgefont = "edgefont"

gclines :: String
gclines = "gclines"

gcfmt :: String
gcfmt = "gcfmt"

gcsizenode :: String
gcsizenode = "gcsizenode"

gcarr :: String
gcarr = "gcarr"

hecol :: String
hecol = "hecol"

evshape :: String
evshape = "evshape"

loopcol :: String
loopcol = "loopcol"

tau :: String
tau = "tau"

breakLoop :: String
breakLoop = "break"

sndm :: String
sndm = "snd"

rcvm :: String
rcvm = "rcv"
