-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- Compute the pomset-based semantics of global graphs.


import Misc
import GGParser
import Data.Set as S
import Data.List as L
import Data.Map.Strict
import PomsetSemantics
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2POM)
            else do
              let ( sourcefile, flags ) = getCmd GG2POM progargs
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gg, _ ) =
                    (gggrammar . GGParser.lexer) ggtxt
              let iter = (read (flags!"-l"))::Int
              let ( pomsets, _ ) =
                    pomsetsOf gg iter 0
              let aux b ps i =
                    let (f, ext) = if flags!"--gml" == "yes"
                                   then (pomset2gml, ".graphml")
                                   else (show, ".txt")
                    in case ps of
                         [] -> return ()
                         p:ps' -> writeToFile (dir ++ b ++ (show i) ++ ext) (f p) >>= \_ -> aux b ps' (i+1)
              aux baseName (S.toList pomsets) 0
