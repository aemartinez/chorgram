--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--

module PetrifyBridge where

import Data.Set as S
import CFSM
import TS
import Misc

--
-- This modules contains function to output files compatible with Petrify
--


state2petrify :: State -> String
state2petrify s = rmChar '\"' (show s)

event2petrify :: (Show a1, Show a) => (State, State, a, a1, Dir, Message) -> String
event2petrify (q1,q2,m1,m2,_,msg) =
  let st_arrow = "AAA"
      st_comma = "CCC"
      st_colon = "COCO"
      st_del   = "delPTP" -- This odd delimiter is to avoid problems with the odd restrictions required by petrify
  in rmChar '\"' (
    (state2petrify q1)++st_comma++
    (state2petrify q2)++st_comma++
    (show m1)++st_del++st_arrow++
    (show m2)++st_del++st_colon++
    (tokenifymsg msg) )

ts2petrify :: TSb -> String -> String
ts2petrify (_, (initconf,_), events, trans) qsep =
  let st_nodes = ".outputs "++(S.fold (++) "" (S.map (\x -> (event2petrify x)++" ") events))++"\n"
      st_init = ".marking {q"++(nodelabel initconf qsep)++"}\n"
      st_trans = S.fold (++) "" (
        S.map (
           \(x,y,z) -> "q" ++ (nodelabel (fst x) qsep)
                       ++" "++
                       (event2petrify y)
                       ++" "++
                       "q" ++ (nodelabel (fst z) qsep)
                       ++"\n"
              )
        trans
        )
  in st_nodes++".state graph \n"++st_trans++st_init++".end\n"
   
