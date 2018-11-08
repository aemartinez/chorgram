import Misc
import DotStuff
import GGparser
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import SemanticGlobalGraphs
import System.Environment

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
              putStrLn $ msgFormat HGSEM "start"
              let ( gg, names ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let ptps =
                    list2map $ S.toList names
              let ( _, hg ) =
                    -- sem (M.member "--sloppy" flags) (-1) fact ptps
                    sem (M.member "--sloppy" flags) (-1) gg ptps
              writeToFile (dir ++ "sem_sgg.dot") (hg2dot hg flines) >>=
                \_ -> putStrLn $ "\t" ++ dir ++ "sem_sgg.dot: is the semantics of the initial gg"
