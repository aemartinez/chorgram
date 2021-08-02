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
              case flags!"--fmt" of
                "gc" -> do
                  let ( gc, _ ) =
                        case gcgrammar gctxt 0 0 of
                          Ok x -> x
                          Er err -> error err
                  writeToFile outfile ("# Input @ " ++ sourcefile ++ "\n\n" ++ show gc)
                  writeToFile outfile (gc2dot gc baseName flines)
                  myPrint flags GC2DOT ("result saved in " ++ outfile)
                "gml" -> do
                  xml <- readFile sourcefile
                  case pomset2gg $ xgml2pomset xml of
                    Nothing ->
                      writeToFile outfile "Nothing"
                    Just gc' -> do
                      writeToFile outfile (gc2dot gc' baseName flines)
                  myPrint flags GC2DOT ("result saved in " ++ outfile)
                "gmldiff" -> do
                  xml <- readFile sourcefile
                  writeToFile outfile (xgmldiff2dot baseName xml flines)
                  myPrint flags GC2DOT ("result saved in " ++ outfile)
                "sloppygml" -> do
                  xml <- readFile sourcefile
                  writeToFile outfile (xgml2dot baseName xml flines)
                  myPrint flags GC2DOT ("result saved in " ++ outfile)
                _ -> error ("ERROR: unknown format " ++ show (flags!"--fmt"))
