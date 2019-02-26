import Misc
import FSA
import SystemParser
import Data.List as L
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(MIN)
            else do
              let flags =
                    getFlags MIN (take ((length progargs) - 1) progargs)
              myPrint flags MIN "parsing started"
              let sourcefile =
                    last progargs
              let filename =
                    takeFileName sourcefile
              txt <- readFile sourcefile
              let (dir, _, _, ext) =
                    setFileNames filename flags
              let _ = lexer txt
              let sys = parseSystem ext txt
              let minSys = L.map minimise (fst sys)
              writeToFile (dir ++ "minimal_" ++ filename ++ ".txt") (show minSys)
              myPrint flags MIN ("\tresult in " ++ dir ++ "minimal_" ++ filename ++ ".txt")
              myPrint flags MIN "done"
