--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module contains the main to generate the choreography
--

import System.Environment
import System.FilePath.Posix
import PetriNet
import Misc
import GlobalGraph
import Data.List as L
import Data.Map

main :: IO ()
main =  do progargs <- getArgs
           if L.null progargs
             then putStrLn $ usage GC
             else do
                  let flags = getFlags GC $ L.take ((length progargs) - 1) progargs
                  let filename = (flags ! "-d") ++ (if (""== (flags ! "-d")) then "" else [pathSeparator]) ++ (takeFileName $ (last progargs))
                  pnfile <- readFile filename
                  let pn = parsePetriNet (Prelude.map (\x -> words x) (lines pnfile))
                  -- myPrint flags GC ("\n" ++ (show pn))
                  myPrint flags GC "Transformation 1: one-Source Petri net"
                  let one_pn = oneSource pn
                  writeToFile (filename ++ "_onesourcepn.dot") (printNet one_pn)
                  myPrint flags GC "Transformation 2: joined Petri net"
                  let joined_pn = (joinPred . joinPost) one_pn
                  writeToFile (filename ++ "_finalpn.dot") (printNet joined_pn)
                  myPrint flags GC "Transformation 3: Petri net to pre-global graph"
                  let pregg =  net2globalgraph joined_pn
                  myPrint flags GC "Transformation 4: pre-global graph to global graph"
                  writeToFile (filename ++ "_preglobal.dot") (globalGraph2String pregg)
                  let gg = cleanupGC pregg
                  writeToFile (filename ++ "_global.dot") (globalGraph2String gg)
                  myPrint flags GC ("Global graph construction done; see " ++ filename)

               
