import Misc
import DotStuff
import GGparser
import CFSM
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import SyntacticGlobalGraphs
import SemanticGlobalGraphs
import BCGBridge
import System.Environment

main :: IO ()
main = do putStrLn $ msgFormat SGG "start"
          progargs <- getArgs
          flines   <- getDotConf
          if L.null progargs
            then error $ usage(SGG)
            else do
              let ( sourcefile, flags ) =
                    (last progargs, getFlags SGG (take ((length progargs) - 1) progargs))
              ggtxt <- readFile sourcefile
              let ( dir, _, baseName, _ ) =
                    setFileNames sourcefile flags
              let ( gg, names ) =
                    (gggrammar . lexer) ggtxt
              let ptps =
                    list2map $ S.toList names
              writeToFile (dir ++ "in_sgg_parsed.txt") (show gg) >>= (\_ -> putStrLn $ "\t"++dir++"in.sgg: is the initial gg")
--              let norm   =
--                    normGG gg
--              let fact =
--                    factorise $ norm
              let fact =
                    factorise $ normGG gg --norm
              let sgg2file s s' =
                    writeToFile (dir ++ s) (gg2dot gg (baseName ++ s') (flines!ggsizenode))
              sgg2file "graph_sgg.dot" ""    >>= \_ -> putStrLn $ "\t" ++ dir ++ "graph_sgg.dot: is the input gg"
              sgg2file "norm_sgg.dot" "norm" >>= \_ -> putStrLn $ "\t" ++ dir ++ "norm_sgg.dot:  is the normalised initial gg"
              sgg2file "fact_sgg.dot" "fact" >>= \_ -> putStrLn $ "\t" ++ dir ++ "fact_sgg.dot:  is the factorised initial gg"
              let ( _, hg ) =
                    sem (-1) fact ptps
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
                     i:ls -> (putStrLn $ "\t" ++ (path i "") ++ " is the machine for participant " ++ (ptps!i)) >>=
                             (\_ -> writeToFile (path i ".dot") (dottifyCfsm cfsm (ptps!i) (legend cfsm i) flines) ) >>=
                             (\_ -> writeToFile (path i ".aut") (cfsm2bcg cfsm flines) ) >>=
                             (\_ -> output ls)
                       where cfsm = fst $ proj gg (ptps!i) "q0" "qe" 0
              output $ range (S.size names)
