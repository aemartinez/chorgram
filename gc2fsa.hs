--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns files with the .fsa format of the minimised
-- CFSMs of a g-choreography both for the single CFSMs and the
-- whole communicating system.
--

import Misc
import GCParser
import CFSM (cfsm2String)
import FSA (minimise)
import SyntacticGlobalGraphs
import System.Environment
import System.Directory(createDirectoryIfMissing)
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GC2FSA)
            else do
              let ( sourcefile, flags ) = getCmd GC2FSA progargs
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              createDirectoryIfMissing True dir
              let ( gg, names ) =
                    (gcgrammar . GCParser.lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              -- TODO: fix this String -> [CFSM] -> [[String]] -> System inefficiency 
              let cfsms =
                    -- Note loops are not projected (1st arg of proj below)
                    L.map (minimise . fst) (L.map (\p -> proj False gg (M.fromList $ L.zip (range $ L.length ptps) ptps) p "q0" "qe" 1) ptps)
              let fsa = L.map (\(p, m) -> (CFSM.cfsm2String p m) ++ "\n\n") (L.zip ptps cfsms)
              if ("" == flags!"-o")
                then putStrLn $ L.concat fsa
                else do
                     mapM_
                       (\(p,m) ->
                          writeToFile (dir ++ baseName ++ (if p=="" then "" else "_cfsm_" ++ p) ++ ".fsa") m)
                       ([("", L.concat fsa)] ++ (L.zip ptps fsa))
----              let hs = L.concat $ L.map (\(p, m) -> "m_" ++ p ++ " = " ++ (show m) ++ "\n\n") (L.zip ptps cfsms)
              if flags!"-v" == ""
                then putStr "" --myPrint flags GC2FSA ("\tresult in " ++ dir ++ baseName ++ ".fsa")
                else ---- writeToFile (dir ++ baseName ++ ".hs") hs
                     ---- >>=
                     ---- \_ ->
                     myPrint flags GC2FSA ("\tThe communicating system and the single CFSMs are in the *.fsa files in " ++ dir)
----                     >>=
----                     \_ -> myPrint flags GC2FSA ("\tThe haskell data structure of the system is in " ++ dir ++ baseName ++ ".hs")


