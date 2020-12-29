--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns the graphml format of .sgg global graph.
--

import Misc
import GCParser
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalChoreographies
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then putStrLn $ usage GC2GML
            else do
              let ( sourcefile, flags ) = getCmd GC2GML progargs
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gg, names ) =
                    (gcgrammar . GCParser.lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              let gml = gc2graphml gg
              if ("" == flags!"-o")
                then putStrLn gml
                else writeToFile (dir ++ baseName) gml


