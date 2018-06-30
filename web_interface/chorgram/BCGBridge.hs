--
-- Author: Emilio Tuosto <emilio@le.ac.uk>
--
-- This module provides the brigde to the CADP tool used to
-- minimise machines. Information bout CADP are available at
-- http://cadp.inria.fr/man/aut.html#sect3 (AUT format; also summarised below)
-- http://cadp.inria.fr/man/bcg_min.html (minimisation)
--
-- The AUT format represents automata in files of the format:
--       des (<initial-state>, h, <number-of-states>)
--       (<from-state_1>, <label_1>, <to-state_1>)
--       ...
--       (<from-state_h>, <label_h>, <to-state_h>)
--
-- where states are positive integers and labels are strings
-- surrounded by '"'. Internal transitions are those with the special
-- label 'i' (without quote symbols).

module BCGBridge where

import CFSM
import Misc
import Data.List as L
import Data.Set as S
import Data.Map.Strict as M
import DotStuff

-- states2int states bijectively associate the set states with the segment of
-- natural numbers 1,...,size(states), after converting numbers into strings
--
states2int :: Set State -> ( Map State String, Map String State )
states2int states = ( f, fInv )
  where f    = M.fromList [( (S.toList states)!!i, show(i + 1) ) | i <- range (S.size states)]
        fInv = M.fromList [( i , q ) | ( q, i ) <- M.assocs f]
    

cfsm2bcg :: CFSM -> Map String String -> String
cfsm2bcg ( states, q0, _, trxs ) flines =
  "des (0, " ++ (show $ S.size trxs) ++ ", " ++ (show $ S.size states) ++ ")\n" ++ (L.concat $ S.toList $ (S.map aux trxs))
  where aux     = \( q, act, q' )  -> "(" ++ f!q ++ ", " ++ labtr act ++ ", " ++ f!q' ++  ")\n"
        f       = M.insert q0 "0" (fst $ states2int $ S.delete q0 states)
        labtr   = \act@(_, _, msg) -> case msg of
                                       '*':'<':_ -> "i"                      -- NOTE: change this if lpref changes
                                       '>':'*':_ -> "i"                      -- NOTE: change this if epref changes
                                       _         -> printAction act flines'
        flines' = M.update (\_ -> Just "i") tau (M.update (\_ -> (Just "::")) "ptpsep" flines)
  
-- bcg2cfsm :: String -> Map String String -> CFSM
-- bcg2cfsm aut flines = ( states, q0, _, trxs )
--   where autlns = linees aut
