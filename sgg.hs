--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- This file contains the main program that supports the pomset
-- and reversible semantics of global graphs.
--
-- At the moment very little is implemented.
--

import Misc
import DotStuff
import GGparser
import RGGparser
import CFSM
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalGraphs
import ErlanGG
import SemanticGlobalGraphs
import BCGBridge
import System.Environment

main :: IO ()
main = do progargs <- getArgs
          flines   <- getDotConf
          if L.null progargs
            then error $ usage(SGG)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags SGG (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              if (M.notMember "-rg" flags)
                then do
                putStrLn $ msgFormat SGG "start"
                let ( gg, names ) =
                      (gggrammar . GGparser.lexer) ggtxt
                let ptps =
                      list2map $ S.toList names
                writeToFile (dir ++ "in_sgg_parsed.txt") ("Input @ " ++ sourcefile ++ "\n\n" ++ show gg)
                  >>= (\_ -> putStrLn $ "\t" ++ dir ++ "in_sgg_parsed.txt: is the abstract syntax tree of the gg in input")
                -- let norm = normGG gg
                -- let fact = factorise norm
                -- let fact = factorise gg
                let sgg2file s s' graph =
                      writeToFile (dir ++ s) (gg2dot graph (baseName ++ s') (flines!ggsizenode))
                sgg2file "graph_sgg.dot" ""  gg   >>= \_ -> putStrLn $ "\t" ++ dir ++ "graph_sgg.dot: is the input gg"
                -- sgg2file "norm_sgg.dot" "norm" norm >>= \_ -> putStrLn $ "\t" ++ dir ++ "norm_sgg.dot:  is the normalised initial gg"
                -- sgg2file "fact_sgg.dot" "fact" fact >>= \_ -> putStrLn $ "\t" ++ dir ++ "fact_sgg.dot:  is the factorised initial gg"
                let ( _, hg ) =
                      -- sem (M.member "--sloppy" flags) (-1) fact ptps
                      sem (M.member "--sloppy" flags) (-1) gg ptps
                writeToFile (dir ++ "sem_sgg.dot") (hg2dot hg flines) >>=
                  \_ -> putStrLn $ "\t" ++ dir ++ "sem_sgg.dot: is the semantics of the initial gg"
                let path i ext =
                      mkFileName (ptps!i) dir "cfsm" ext
                let legend m i =
                      if (M.notMember "-l" flags)
                      then "subgraph legend {\n\t#rank = sink;\n\tLegend [shape=rectangle, penwidth=0, fontname=courier, fontsize=8, fillcolor=gray94, style=filled, fontcolor=coral, margin=0.1,\n\t\tlabel="
                           ++    "\"Source file          : " ++ "cfsm" ++ (rmChar '\"' $ show $ ptps!i) ++ ".dot"
                           ++ "\t\\lDestination dir      : " ++ dir
                           ++ "\t\\lNumber of states     : " ++ (show $ (S.size $ statesOf m))
                           ++ "\t\\lNumber of transitions: " ++ (show $ (S.size $ transitionsOf m))
                           ++ "\\l\"];\n}"
                      else ""
                let output l =
                      case l of
                        []   -> putStrLn $ msgFormat SGG "end"
                        i:ls -> (putStrLn $ "\t" ++ (path i "") ++ " is the machine for participant " ++ (ptps!i)) >>= (\_ -> writeToFile (path i ".dot") (dottifyCfsm cfsm (ptps!i) (legend cfsm i) flines) ) >>= (\_ -> writeToFile (path i ".aut") (cfsm2bcg cfsm flines) ) >>= (\_ -> output ls)
                          where cfsm = fst $ proj gg (ptps!i) "q0" "qe" 0
                output $ range (S.size names)
                else do
                -- TODO: the compilation to erlang should be better integrated with the rest
                let (rgg, names) =
                      (rgggrammar . RGGparser.lexer) ggtxt
                writeToFile (dir ++ "in_rgg_parsed.txt") (show rgg)
                  >>=
                  (\_ -> writeToFile (dir ++ "reg.txt") (if (head $ fst (rgg2erl 1 rgg)) == '[' then fst (rgg2erl 1 rgg) else erlList $ fst (rgg2erl 1 rgg)))
