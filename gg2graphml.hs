--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- This main returns the graphML files of the pomsets of a given global graph.
--

import Misc
import GGparser
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M
import SemanticGlobalGraphs as Sem
import System.Environment
import Control.Monad (foldM)
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2GML)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GG2GML (take ((length progargs) - 1) progargs))
              let iter = read (flags!"-l") :: Int
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gg, _ ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let pomsets =
                    Data.Set.toList $ fst $ Sem.pomsetsOf gg iter 0
              foldM
                (\_ (gml,i) -> (writeToFile (dir ++ (show i) ++ "_" ++ baseName ++ ".graphml") gml))
                ()
                (L.zip (L.map pomset2GML pomsets) (range $ L.length pomsets))
              myPrint flags GG2GML ("\tresult in " ++ dir ++ "*_" ++ baseName ++ ".graphml")


