--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This main returns the fsa or dot format of the projections of a
-- g-choreography both for the single CFSMs and the whole
-- communicating system
--

import Misc
import DotStuff (getDotConf)
import GCParser (gcgrammar, lexer)
import CFSM (cfsm2String, emptyCFSM, dottifyCfsm, printCfsm)
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
                      handleND =
                        case flags!"-D" of
                          "min" -> (minimise . fst)
                          "det" -> (determinise . fst)
                          _ -> fst
                      output =
                        if ptp=="all"
                        then
                          let cs =
                                case (flags!"--fmt") of
                                  "fsa" -> L.map (\p -> CFSM.cfsm2String p (handleND $ proj gc "q0" "qe" p loops ptps_map)) ptps
                                  "dot" -> L.map (\(p,s) -> "\nsubgraph " ++ p ++ "{\n label=\"" ++ p ++ "\"\n" ++ s ++ "\n}")
                                                 (L.map (\p -> (p, CFSM.printCfsm (handleND $ proj gc "q0" "qe" p loops ptps_map) p flines)) ptps)
                                  _ -> error $ msgFormat PROJ ("unknown format " ++ (flags!"--fmt"))
                              (pre,post) =
                                case (flags!"--fmt") of
                                  "fsa" -> ("", "")
                                  "dot" -> ("digraph all{\n", "\n}")
                                  _ -> error $ msgFormat PROJ ("unknown format " ++ (flags!"--fmt"))
                          in pre ++ (L.foldr (++) "" cs) ++ post
                        else
                          case L.elemIndex ptp ptps of
                            Nothing ->
                              if (flags!"--fmt") == "fsa"
                              then CFSM.cfsm2String ptp emptyCFSM
                              else dottifyCfsm emptyCFSM ptp "" flines
                            Just _ ->
                              case (flags!"--fmt") of
                                "fsa" -> CFSM.cfsm2String ptp (handleND (proj gc "q0" "qe" ptp loops ptps_map))
                                "dot" -> dottifyCfsm (handleND (proj gc "q0" "qe" ptp loops ptps_map)) ptp "" flines
                                _ -> error $ msgFormat PROJ ("unknown format " ++ (flags!"--fmt"))
                  putStrLn output
                else do mapM_ (\(k,v) -> putStrLn $ (show k) ++ " |--> " ++ (show v)) (M.toList ptps_map)

