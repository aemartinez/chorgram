import Misc
import FSA
import SystemParser
import Data.List as L
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do putStrLn "minimise: parsing started"
          progargs <- getArgs
          if L.null progargs
            then error $ usage(MIN)
            else do
              let flags =
                    getFlags MIN (take ((length progargs) - 1) progargs)
              let sourcefile =
                    last progargs
              let filename =
                    takeFileName sourcefile
              txt <- readFile sourcefile
              let (dir, _, _, _) =
                    setFileNames filename flags
              let _ = lexer txt
              let sys =
                    (sysgrammar . lexer) txt
              let minSys = L.map minimise (fst sys)
              writeToFile (dir ++ "minimal" ++ filename ++ ".txt") (show minSys)
              putStrLn ("minimise:\tresult in " ++ dir ++ "minimal" ++ filename ++ ".txt")
              putStrLn "minimise: done"
