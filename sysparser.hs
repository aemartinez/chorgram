--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main parses a communicating system and prints
-- its corresponding Haskell's data structure
--

import Misc
import SystemParser
import Data.List as L
import System.Environment
import System.FilePath.Posix

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then putStr $ usage SYS
            else do
              let flags =
                    getFlags SYS (take ((length progargs) - 1) progargs)
              myPrint flags SYS "parsing started"
              let sourcefile =
                    last progargs
              let filename =
                    takeFileName sourcefile
              txt <- readFile sourcefile
              let (_, _, _, ext) =
                    setFileNames filename flags
              let sys = parseSystem ext txt
              putStr $ show sys
