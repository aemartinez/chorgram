import Misc
import SystemParser
import Data.List as L
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do putStrLn "systemparser: parsing started"
          progargs <- getArgs
          if L.null progargs
            then error $ usage(SYS)
            else do
              let flags =
                    getFlags SYS (take ((length progargs) - 1) progargs)
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
              writeToFile (dir ++ "parsed" ++ filename ++ ".txt") (show sys)
              putStrLn ("systemparser:\tresult in " ++ dir ++ "parsed" ++ filename ++ ".txt")
              putStrLn "systemparser: parsing ended"
