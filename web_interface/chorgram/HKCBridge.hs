--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--
-- This module provides the brigde to HKC

module HKCBridge where

import CFSM
import Misc
import Data.Set as S


timbukStatePref :: String
timbukStatePref = "qtimbuk"

-- HKC does not support numbers in symbols
channel2timbuk :: ( Ptp, Ptp ) -> String
channel2timbuk ( s, r ) = s ++ r

state2timbuk :: State -> String
state2timbuk s = rmChar '\"' $ aux (show s)
  where aux = \s -> case s of
                     []     -> []
                     '_':s' -> "UUU" ++ (aux s')
                     x:s'   -> x:(aux s')

action2timbuk :: Action -> String
action2timbuk ( dir, chan, msg ) = rmChar '\"' $
  case dir of
    Send    -> (channel2timbuk chan) ++ "SEND" ++ (tokenifymsg msg)
    Receive -> (channel2timbuk chan) ++ "RECEIVE" ++ (tokenifymsg msg)
    Tau     -> error "Non minimised machine"
    LoopSnd -> (channel2timbuk chan) ++ "SEND" ++ (tokenifymsg msg)
    LoopRcv -> (channel2timbuk chan) ++ "RECEIVE" ++ (tokenifymsg msg)

trans2timbuk :: LTrans -> String
trans2timbuk ( q, action, q' ) =
  (action2timbuk action) ++ "(" ++ (state2timbuk q) ++ ")" ++ " -> " ++ (state2timbuk q') ++ "\n"

cfsm2timbuk :: CFSM -> Ptp -> String
cfsm2timbuk (states, initstate, actions, trans) ptp =
  let stactions = S.fold (++) "" (S.map (\x -> (action2timbuk x) ++ ":1 ") actions)
      liststates = S.fold (++) "" (S.map (\x -> (state2timbuk x) ++ " ") states)
      header = "Ops xxxx:0 " ++ stactions ++ "\n \n"
      name = "Automaton " ++ ptp ++ "\n"
      ststates = "States " ++ liststates ++ "\n"
      finalstates = "Final States " ++ liststates ++ "\n"
      sttrans = "Transitions \n" ++
                "xxxx() -> " ++ (state2timbuk initstate) ++ "\n"++
                S.fold (++) "" (S.map (\x -> (trans2timbuk x)) trans)
   in header ++ name ++ ststates ++ finalstates ++ sttrans
      
      
