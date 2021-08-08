--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
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
main = do
  progargs <- getArgs
  if L.null progargs
    then putStrLn $ usage MIN
    else do
      let
        flags =
          getFlags MIN (L.take ((length progargs) - 1) progargs)
      flines <- getDotConf
      myPrint flags MIN "started"
      let sourcefile =
            last progargs
      let filename =
            takeFileName sourcefile
      txt <- readFile sourcefile
      let (_, _, basename, ext) =
            setFileNames filename flags
      let _ = lexer txt
      let (sys,  ptps) = parseSystem ext txt
      let (sys', f) =
            case (flags!"-D") of
              "min" -> (L.map FSA.minimise sys, basename ++ ".min")
              "det" -> (L.map FSA.determinise sys, basename ++ ".det")
              "no"  -> (sys, basename ++ ".hs")
              _     -> error ("value " ++ (flags!"-D") ++ " not appropriate for flag -D; use \"min\", \"det\", or \"no\"" )
      -- writeToFile f (show sys')
      putStrLn $ show $  ptps
      putStrLn $ show $ M.elems flags
      writeToFile (basename ++ flags!"-D" ++ ".fsa") (system2String (sys', ptps))
      writeToFile (basename ++ ".dot") (dottifySystem flines (sys', ptps))
      myPrint flags MIN ("\tresult in " ++ f ++ ", " ++ basename ++ ".dot, and in " ++ basename ++ flags!"-D" ++ ".fsa")
