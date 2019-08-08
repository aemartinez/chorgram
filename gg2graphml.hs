--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- This main returns the graphML files of the pomsets of a given global graph.
--

import Misc
import GGparser
import Data.Set (toList,map)
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalGraphs
import SemanticGlobalGraphs as Sem
import System.Environment
import Control.Monad (foldM)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2GML)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GG2GML (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              let ( gg, _ ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let pomsets =
                    Data.Set.toList $ fst $ Sem.pomsetsOf False 0 gg
              foldM
                (\_ (gml,i) -> (writeToFile (dir ++ (show i) ++ "_" ++ baseName ++ ".graphml") gml))
                ()
                (L.zip (L.map pomset2GML pomsets) (range $ L.length pomsets))
              myPrint flags GG2GML ("\tresult in " ++ dir ++ "*_" ++ baseName ++ ".graphml")


