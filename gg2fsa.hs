--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- This main returns a file with the .fsa format of the minimised
-- CFSMs of a global graph.
--

import Misc
import GGparser
import CFSM (cfsm2String)
import FSA (minimise)
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalGraphs
import System.Environment

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then error $ usage(GG2FSA)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags GG2FSA (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              let ( gg, names ) =
                    (gggrammar . GGparser.lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              -- TODO: fix this String -> [CFSM] -> [[String]] -> System inefficiency 
              let cfsms =
                    L.map (minimise . fst) (L.map (\p -> proj False gg (M.fromList $ L.zip (range $ L.length ptps) ptps) p "q0" "qe" 1) ptps)
              -- let sys =
              --       CFSM.parseFSA ((L.map (\(i,m) -> lines (CFSM.cfsm2String i m))) (L.zip ptps cfsms))
              writeToFile (dir ++ baseName ++ ".fsa") (L.concat $ L.map (\(p, m) -> (CFSM.cfsm2String p m) ++ "\n\n") (L.zip ptps cfsms))
              if not(flags!"-v" == "")
                then writeToFile (dir ++ baseName ++ ".hs")  (L.concat $ L.map (\(p, m) -> "m_" ++ p ++ " = " ++ (show m) ++ "\n\n") (L.zip ptps cfsms))
                     >>=
                     \_ -> myPrint flags GG2FSA ("\tresult in " ++ dir ++ baseName ++ ".fsa and in " ++ dir ++ baseName ++ ".hs")
                else myPrint flags GG2FSA ("\tresult in " ++ dir ++ baseName ++ ".fsa")


