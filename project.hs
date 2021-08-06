--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This program returns the fsa or dot format of projections of a
-- g-choreography. It is possible to select the participants onto
-- which projections must be calculated. A typical usage is
--
--    project <filename> -p A B C
--
-- where <filename> is the path to a .gc file with a choreography
-- having A, B, and C among its participants. If the -p option is
-- omitted, all the projections are returned.
-- 

import Misc
import DotStuff (getDotConf)
import GCParser
import CFSM (cfsm2String, emptyCFSM, dottifyCFSM, printCFSM, prettyDotCFSM)
import FSA (minimise,determinise)
import SyntacticGlobalChoreographies (proj)
import System.Environment
import Data.Set (toList)
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do
  progargs <- getArgs
  flines <- getDotConf
  if L.null progargs
    then do putStrLn $ usage PROJ
    else do
      let tmp =
            L.dropWhile (\x -> x /= "-p") progargs
      let ptp =
            if L.null tmp then [] else tail tmp
      let ( sourcefile, flags ) =
            getCmd PROJ (L.take (L.length progargs - L.length tmp) progargs)
      gctxt <- readFile sourcefile
      let ( gc, names ) =
            case gcgrammar gctxt 0 0 of
              Ok x -> x
              Er err -> error err
      let ptps = Data.Set.toList names
      let ptps_map =
            M.fromList $ L.zip (range $ L.length ptps) ptps
      let loops =
            (read (flags!"-u"))::Int
      let handleND =
            case flags!"-D" of
              "min" -> (minimise . fst)
              "det" -> (determinise . fst)
              _ -> fst
      let (pre,post) =
            case (flags!"--fmt") of
              "fsa" -> ("", "")
              "dot" -> ("digraph all{\n", "\n}")
              _ -> error $ msgFormat PROJ ("unknown format " ++ (flags!"--fmt"))
      let output =
            if ptp==[] -- all projections are returned
            then
              let cs =
                    case (flags!"--fmt") of
                      "fsa" -> L.map (\p -> CFSM.cfsm2String p (handleND $ proj gc "q0" "qe" p loops ptps_map)) ptps
                      "dot" -> L.map (\(p,s) -> "\nsubgraph " ++ p ++ "{\n label=\"" ++ p ++ "\"\n" ++ s ++ "\n}")
                        (L.map (\p -> (p, CFSM.prettyDotCFSM (handleND $ proj gc "q0" "qe" p loops ptps_map) p flines ptps_map)) ptps)
                      _ -> error $ msgFormat PROJ ("unknown format " ++ (flags!"--fmt"))
              in
                pre ++ (L.foldr (++) "" cs) ++ post
            else
              let aux p =
                    case L.elemIndex p ptps of
                      Nothing ->
                        if (flags!"--fmt") == "fsa"
                        then CFSM.cfsm2String p emptyCFSM
                        else dottifyCFSM emptyCFSM p "" flines
                      Just _ ->
                        case (flags!"--fmt") of
                          "fsa" -> CFSM.cfsm2String p (handleND (proj gc "q0" "qe" p loops ptps_map))
                          "dot" ->
                            CFSM.prettyDotCFSM (handleND (proj gc "q0" "qe" p loops ptps_map)) p flines ptps_map
                          _ -> error $ msgFormat PROJ ("unknown format " ++ (flags!"--fmt"))
              in
                L.foldl (\x y -> x ++ "\n\n" ++ (aux y)) "" ptp
      putStrLn (pre ++ output ++ post)
      if (flags!"-v" /= "")
        then do mapM_ (\(k,v) -> putStrLn $ (show k) ++ " |--> " ++ (show v)) (M.toList ptps_map)
        else do putStrLn ""
