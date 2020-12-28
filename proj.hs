--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns files with the .fsa format of the minimised
-- CFSMs of a g-choreography both for the single CFSMs and the
-- whole communicating system.
--

import Misc
import GCParser (gcgrammar, lexer)
import CFSM (cfsm2String, emptyCFSM)
import FSA (minimise,determinise)
import SyntacticGlobalGraphs (proj)
import System.Environment
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do progargs <- getArgs
          if L.null progargs
            then putStrLn $ usage PROJ
            else do
              let ptp =
                    L.last progargs
              let ( sourcefile, flags ) =
                    getCmd PROJ (L.take (L.length progargs - 1) progargs)
              ggtxt <- readFile sourcefile
              let ( gg, names ) =
                    (gcgrammar . lexer) ggtxt
              let ptps =
                    Data.Set.toList names
              let ptps_map =
                    M.fromList $ L.zip (range $ L.length ptps) ptps
              let loopflag =
                    (read (flags!"-l"))::Bool
              let cfsm =
                    case L.elemIndex ptp ptps of
                      Nothing -> emptyCFSM -- error $ msgFormat PROJ (ptp ++ " does not belong to " ++ (show ptps))
                      Just _ ->
                        (case flags!"-D" of
                           "min" -> (minimise . fst)
                           "det" -> (determinise . fst)
                           _ -> fst
                        ) (proj loopflag gg ptps_map ptp "q0" "qe" 1)
               in putStrLn $ CFSM.cfsm2String ptp cfsm


