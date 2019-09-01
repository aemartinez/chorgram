--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns the graphml format of .sgg global graph.
--

import Misc
import GGparser
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalGraphs
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2GML)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GMC (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gg, names ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              let gml = gg2graphml gg
              putStrLn gml  -- to be removed
              writeToFile (dir ++ baseName ++ ".graphml") gml


