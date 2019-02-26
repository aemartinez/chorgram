import Misc
import SystemParser
import Data.List as L
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(SYS)
            else do
              let flags =
                    getFlags SYS (take ((length progargs) - 1) progargs)
              myPrint flags SYS "parsing started"
              let sourcefile =
                    last progargs
              let filename =
                    takeFileName sourcefile
              txt <- readFile sourcefile
              let (dir, _, _, ext) =
                    setFileNames filename flags
--              let _ = lexer txt
--              let sys =
--                    (sysgrammar . lexer) txt
              let sys = parseSystem ext txt
              writeToFile (dir ++ "parsed_" ++ filename ++ ".txt") (show sys)
              myPrint flags SYS ("result in " ++ dir ++ "parsed_" ++ filename ++ ".txt")
              myPrint flags SYS "parsing done"
