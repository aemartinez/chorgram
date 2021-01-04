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
              ggtxt <- readFile sourcefile
              let ( _, _, baseName, _ ) =
                    setFileNames sourcefile flags
              let dir = flags!"-d"
              createDirectoryIfMissing True dir
              case flags!"--fmt" of
                "gc" -> do
                  let ( gg, _ ) =
                        (gcgrammar . GCParser.lexer) ggtxt
                  writeToFile (dir ++ baseName ++ ".dot") ("# Input @ " ++ sourcefile ++ "\n\n" ++ show gg)
                  writeToFile (dir ++ baseName ++ ".dot") (gc2dot gg baseName (flines!ggsizenode))
                "gml" -> do
                  xml <- readFile sourcefile
                  case pomset2gg $ xgml2pomset xml of
                    Nothing -> writeToFile (dir ++ baseName ++ ".dot") "Nothing"
                    Just gg' -> writeToFile (dir ++ baseName ++ ".dot") (gc2dot gg' baseName sizeNode)
                "gmldiff" -> do
                  xml <- readFile sourcefile
                  writeToFile (dir ++ baseName ++ ".dot") (xgmldiff2dot baseName xml flines)
                "sloppygml" -> do
                  xml <- readFile sourcefile
                  writeToFile (dir ++ baseName ++ ".dot") (xgml2dot baseName xml flines)
                _ -> error ("ERROR: unknown format " ++ show (flags!"--fmt"))