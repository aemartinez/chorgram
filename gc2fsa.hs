--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns minimised CFSMs in the fsa format of a
-- g-choreography both for the single CFSMs and the whole
-- communicating system.
--

import Misc
import GCParser
import CFSM (cfsm2String)
import FSA (minimise)
import SyntacticGlobalChoreographies
import System.Environment
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then putStrLn $ usage GC2FSA
            else do
              let ( sourcefile, flags ) =
                    getCmd GC2FSA progargs
              ggtxt <- readFile sourcefile
              let ( gg, names ) =
                    (gcgrammar . GCParser.lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              -- TODO: fix this String -> [CFSM] -> [[String]] -> System inefficiency
              let ptp_index =
                    M.fromList $ L.zip (range $ L.length ptps) ptps
              let cfsms =
                    -- Note loops are not projected (1st arg of projx below)
                    L.map (minimise . fst) (L.map (\p -> projx False gg ptp_index p "q0" "qe" 1) ptps)
              let fsa = L.map (\(p, m) -> (CFSM.cfsm2String p m) ++ "\n\n") (L.zip ptps cfsms)
              if ("" == flags!"-o")
                then putStrLn $ L.concat fsa
                else do
                     mapM_
                       (\(p,m) ->
                          writeToFile ((flags!"-o") ++ (if p=="" then "" else "_cfsm_" ++ p) ++ ".fsa") m)
                       ([("", L.concat fsa)] ++ (L.zip ptps fsa))

