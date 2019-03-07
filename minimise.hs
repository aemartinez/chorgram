--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- Minimisation of systems of cfsms.
--
import Misc
import FSA
import CFSM (system2String,dottifySystem)
import DotStuff
import SystemParser
import Data.List as L
import Data.Map.Strict as M
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(MIN)
            else do
              let flags =
                    getFlags MIN (take ((length progargs) - 1) progargs)
              flines <- getDotConf
              myPrint flags MIN "started"
              let sourcefile =
                    last progargs
              let filename =
                    takeFileName sourcefile
              txt <- readFile sourcefile
              let (_, _, _, ext) =
                    setFileNames filename flags
              let _ = lexer txt
              let (sys,  ptps) = parseSystem ext txt
              let (sys', f) = case (flags!"-D") of
                                "min" -> (L.map FSA.minimise sys, filename ++ ".min")
                                "det" -> (L.map FSA.determinise sys, filename ++ ".det")
                                "no"  -> (sys, filename ++ ".txt")
                                _     -> error ("value " ++ (flags!"-D") ++ " not appropriate for flag -D; use \"min\", \"det\", or \"no\"" )
              writeToFile f (show sys')
              writeToFile (filename ++ ".fsa") (system2String (sys', ptps))
              writeToFile (filename ++ ".dot") (dottifySystem flines (sys, ptps))
              myPrint flags MIN ("\tresult in " ++ f ++ " and in " ++ filename ++ ".fsa")
              myPrint flags MIN "done"
