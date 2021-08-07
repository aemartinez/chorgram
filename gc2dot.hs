--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This file converts gc or graphml files of choreographies in dot
--

import Misc
import DotStuff
import GCParser
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalChoreographies
import PomsetSemantics
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          flines <- getDotConf
          if L.null progargs
            then putStrLn $ usage GC2DOT
            else do
              let ( sourcefile, flags ) = getCmd GC2DOT progargs
              gctxt <- readFile sourcefile
              let ( _, _, baseName, _ ) =
                    setFileNames sourcefile flags
              let dir = flags!"-d"
              let outfile = dir ++ baseName ++ ".dot"
              createDirectoryIfMissing True dir
              let output =
                    if flags!"-o" == ""
                    then putStrLn
                    else writeFile outfile
              case flags!"--fmt" of
                "gc" -> do
                  let ( gc, _ ) =
                        case gcgrammar gctxt 0 0 of
                          Ok x -> x
                          Er err -> error err
                  output ("# Input @ " ++ sourcefile ++ "\n\n# " ++ show gc)
                  output (gc2dot gc baseName flines)
                "gml" -> do
                  xml <- readFile sourcefile
                  case pomset2gg $ xgml2pomset xml of
                    Nothing ->
                      output "Nothing"
                    Just gc' -> do
                      output (gc2dot gc' baseName flines)
                "gmldiff" -> do
                  xml <- readFile sourcefile
                  output (xgmldiff2dot baseName xml flines)
                "sloppygml" -> do
                  xml <- readFile sourcefile
                  output (xgml2dot baseName xml flines)
                _ -> error ("ERROR: unknown format " ++ show (flags!"--fmt"))

              
