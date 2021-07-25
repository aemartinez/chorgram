-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- Compute the pomset-based semantics of global graphs.


import Misc
import GCParser
import Data.Set as S
import Data.List as L
import Data.Map.Strict
import PomsetSemantics
import System.Environment
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then putStrLn $ usage GC2POM
            else do
              let ( sourcefile, flags ) = getCmd GC2POM progargs
              gctxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gc, _ ) =
                    case gcgrammar gctxt 0 0 of
                      Ok x -> x
                      Er err -> error err
              let iter = (read (flags!"-u"))::Int
              let ( pomsets, _ ) =
                    pomsetsOf gc iter 0
              let aux b ps i =
                    let (f, ext) =
                          case flags!"--fmt" of
                            "gml" -> (pomset2gml, ".graphml")
                            "hs"  -> (show, ".hs")
                            _ -> error $ usage GC2POM
                    in case ps of
                         [] -> return ()
                         p:ps' -> writeToFile (dir ++ b ++ (show i) ++ ext) (f p) >>= \_ -> aux b ps' (i+1)
              aux baseName (S.toList pomsets) 0
              if (flags!"-v" == "")
                then putStrLn ""
                else myPrint flags GC2POM ("\tThe results are in " ++ dir)
