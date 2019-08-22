# Authors: Emilio Tuosto <emilio@le.ac.uk>
#
# Compute the pomset-based semantics of global graphs.


import Misc
import GGparser
import Data.Set as S
import Data.List as L
import SemanticGlobalGraphs
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2POM)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GG2POM (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gg, _ ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let ( pomsets, _ ) =
                    pomsetsOf gg 0 0
              let aux b ps i = case ps of
                                 [] -> return ()
                                 p:ps' -> writeToFile (dir ++ b ++ (show i) ++ ".graphml") (pomset2GML p) >>= \_ -> aux b ps' (i+1)
              aux baseName (S.toList pomsets) (0 :: Int)