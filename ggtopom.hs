import Misc
import GGparser
import Data.Set as S
import Data.List as L
import SemanticGlobalGraphs
import System.Environment

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(HGSEM)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags HGSEM (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              putStrLn $ msgFormat HGSEM "start"
              let ( gg, _ ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let ( pomsets, _ ) =
                    pomsetsOf gg 0 0
              let aux b i ps = case ps of
                                 [] -> putStrLn $ "\t" ++ "sem_sgg.dot: is the semantics of the initial gg"
                                 p:ps' -> writeToFile (dir ++ b ++ (show i) ++ ".graphml") (pomset2GML p) >>= \_ -> aux b (i+1) ps'
              aux baseName 0 (S.toList pomsets)
