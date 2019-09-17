-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- yields the hypergraph-based semantics of global graphs.

import Misc
import DotStuff
import GGParser
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import HGSemantics
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          flines   <- getDotConf
          if L.null progargs
            then error $ usage(HGSEM)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags HGSEM (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              myPrint flags HGSEM "start"
              let ( gg, names ) =
                    (gggrammar . GGParser.lexer) ggtxt
              let ptps =
                    list2map $ S.toList names
              let ( _, hg ) =
                    -- sem (M.member "--sloppy" flags) (-1) fact ptps
                    sem (M.member "--sloppy" flags) (-1) gg ptps
              writeToFile (dir ++ "sem_sgg.dot") (hg2dot hg flines)
                >>=
                \_ -> myPrint flags SGG ("\t" ++ dir ++ "sem_sgg.dot: is the semantics of the initial gg")
