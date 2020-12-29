--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns files with the .fsa format of the minimised
-- CFSMs of a g-choreography both for the single CFSMs and the
-- whole communicating system.
--

import Misc
import DotStuff (getDotConf)
import GCParser (gcgrammar, lexer)
import CFSM (cfsm2String, emptyCFSM, dottifyCfsm)
import FSA (minimise,determinise)
import SyntacticGlobalChoreographies (proj)
import System.Environment
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do progargs <- getArgs
          flines <- getDotConf
          if L.null progargs
            then do putStrLn $ usage PROJ
            else do
              let ptp =
                    L.last progargs
              let ( sourcefile, flags ) =
                    getCmd PROJ (L.take (L.length progargs - 1) progargs)
              gctxt <- readFile sourcefile
              let ( gc, names ) =
                    (gcgrammar . lexer) gctxt
              let ptps =
                    Data.Set.toList names
              let ptps_map =
                    M.fromList $ L.zip (range $ L.length ptps) ptps
              if (flags!"-v" == "")
                then do 
                  let loops =
                        (read (flags!"-u"))::Int
                      cfsm =
                        case L.elemIndex ptp ptps of
                          Nothing -> emptyCFSM
                          Just _ ->
                            (case flags!"-D" of
                               "min" -> (minimise . fst)
                               "det" -> (determinise . fst)
                               _ -> fst
                            ) (proj gc "q0" "qe" ptp loops ptps_map)
                  putStrLn $ --CFSM.cfsm2String ptp cfsm ++
                    "\n\n" ++ (CFSM.dottifyCfsm cfsm ptp "" flines)
                else do mapM_ (\(k,v) -> putStrLn $ (show k) ++ " |--> " ++ (show v)) (M.toList ptps_map)

