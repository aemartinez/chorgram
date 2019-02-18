--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- This main returns a file with the .fsa format of the CFSMs of a
-- global graph.
--

import Misc
import GGparser
import CFSM (cfsm2String)
import FSA (minimise)
import Data.Set (toList)
import Data.List as L
import SyntacticGlobalGraphs
import System.Environment

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2FSA)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GG2FSA (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              let ( gg, names ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              let cfsms =
                    L.map (minimise . fst) (L.map (\p -> proj gg p "q0" "qe" 1) ptps)
              let fsafmt =
                    L.map (\(i,m) -> CFSM.cfsm2String i m ++ "\n\n") (L.zip ptps cfsms)
              writeToFile (dir ++ baseName ++ ".fsa") (L.concat fsafmt)
