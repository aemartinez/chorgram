-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- Compute the global graph representing a pomset, if it exists


import Misc
import Data.List as L
import PomsetSemantics
import SyntacticGlobalGraphs
import DotStuff
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2POM)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GG2POM (take ((length progargs) - 1) progargs))
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              xml <- readFile sourcefile
              let pomset = xgml2pomset xml
              let gg =
                    pomset2gg pomset
              -- let aux b ps i =
              --       let (f, ext) = if flags!"--gml" == "yes"
              --                      then (pomset2gml, ".graphml")
              --                      else (show, ".txt")
              --       in case ps of
              --            [] -> return ()
              --            p:ps' -> writeToFile (dir ++ b ++ (show i) ++ ext) (f p) >>= \_ -> aux b ps' (i+1)
              -- aux baseName (S.toList pomsets) (0 :: Int)
              case gg of
                Nothing -> putStrLn "The pomset in not representable as global graph"
                Just gg' ->
                  writeToFile (dir ++ baseName ++ ".gml") (gg2graphml gg') >>=
                  \_ -> writeToFile (dir ++ baseName ++ ".dot") (gg2dot gg' baseName sizeNode)
