--
-- Author: Emilio Tuosto <emilio@le.ac.uk>
--
-- This module contains function to map GG and HG to DOT
--

module DotStuff where

import Data.List as L
import Data.Map.Strict as M
import qualified Data.Text as T

dotCFG :: String
dotCFG = "aux/dot.cfg"

setDOT :: String -> Map String String
setDOT conf = M.fromList $ L.concat $ L.map (\l -> L.map (\p -> (T.unpack $ p!!0, T.unpack $ p!!1)) [T.words l]) (T.lines $ T.pack conf)


getDotConf :: IO(Map String String)
getDotConf = do
             conf <- readFile dotCFG
             let lines = T.lines $ T.pack conf
             let aux   = \l -> L.map (\p -> (T.unpack $ p!!0, T.unpack $ p!!1)) [T.words l | ((L.length $ T.unpack l) > 2) && (L.take 2 (T.unpack l) /= "--")]
             return (M.fromList $ L.concat $ L.map aux lines)

cpV :: String
cpV = " [label=\"\", shape=point, width=0.15, height=0.15, color=darkorange, fillcolor=darkorange, style=filled]\n"

heV :: String
heV = " [label=\"\", shape=square, width=0.05, height=0.05, color=blue, fillcolor=whitesmoke, style=filled]\n"

forkV :: String
forkV = " [label=\"|\", shape=square, fixedsize=true,fillcolor=papayawhip,style=filled,fontcolor=sienna]\n"

joinV :: String
joinV = " [label=\"|\", shape=square, fixedsize=true,fillcolor=sienna,style=filled,fontcolor=papayawhip]\n"

branchV :: String
branchV = " [label=\"+\", shape=diamond, fixedsize=true,fillcolor=papayawhip,style=filled,fontcolor=sienna]\n"

mergeV :: String
mergeV = " [label=\"+\", shape=diamond, fixedsize=true,fillcolor=sienna,style=filled,fontcolor=papayawhip]\n"

sourceV :: String
sourceV = " [label=\"\", shape=circle, fixedsize=true]\n"

sinkV :: String
sinkV = " [label=\"\", shape=circle, width=0.15, height=0.15, fixedsize=true, peripheries=2]\n"

sizeNode :: String
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

ggnodesep :: String
ggnodesep = "ggnodesep"

nodefont :: String
nodefont = "nodefont"

edgefont :: String
edgefont = "edgefont"

gglines :: String
gglines = "gglines"

ggfmt :: String
ggfmt = "ggfmt"

ggsizenode :: String
ggsizenode = "ggsizenode"

ggarr :: String
ggarr = "ggarr"

hecol :: String
hecol = "hecol"

evshape :: String
evshape = "evshape"

loopcol :: String
loopcol = "loopcol"

tau :: String
tau = "tau"

sndm :: String
sndm = "snd"

rcvm :: String
rcvm = "rcv"
