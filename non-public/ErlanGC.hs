--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module contains function to project a GC & REG to Erlang
--

module ErlanGC where

import SyntacticGlobalChoreographies
import Misc
import CFSM
import Data.List as L
import Data.Map.Strict as M
import Data.Char

-- A syntactic reversible global graph is like a syntactic global
-- graph with guards and selectors on branches; for simplicity we
-- consider just binary parallel and branches
data RGC = Pme
         | Tca Channel Message
         | Rap [RGC]
         | Arb Ptp [(RGC, ReversionGuard)]
         | Qes [RGC]
         | Per Ptp RGC ReversionGuard
         deriving (Eq, Ord, Show)

type Endpoint = String
type Guard = String
type ReversionGuard = Map Ptp Guard


erlTuple :: [String] -> String
erlTuple tuple = case tuple of
  [] -> ""
  _  -> "{ " ++ (mkSep tuple ", ") ++ " }"

erlList :: String -> String
erlList els = if (head els) == '[' then els else "[ " ++ els ++ " ]"
-- erlList els = "[ " ++ els ++ " ]"

erlAtom :: String -> String -> String
erlAtom pre s = case s of
  "" -> ""
  _  -> if isLower(head s) then s else pre ++ s

guard2erl :: ReversionGuard -> String
guard2erl g = if M.null g then "\" \"" else "\"" ++ show g ++ "\""
--
-- rgc2erl ln _rgg generates a string encoding _rgg in Erlang's format
--         for the REGs' syntax
-- Pre: Branches must me decorated with the selector and guards
--      must be Erlang expressions and ln is a strictly positive integer
-- Post: a string in the format expected by encoding.erl
--
rgc2erl :: Int -> RGC -> (String, Int)
rgc2erl ln _rgg =
  let sep = ", "
  in case _rgg of
    Tca (s,r) m -> (erlTuple [show ln, erlTuple ["com", erlAtom "ptp_" s, erlAtom "ptp_" r, erlAtom "msg_" m] ], 1 + ln)
    Rap rggs ->
      let aux = \rg -> \(t, l) ->
            let (t', l') = rgc2erl l rg in
            case (t, t') of
              ("","") -> ("", l')
              ("", _) -> (erlList t', l')
              (_ , _) -> (erlList t' ++ sep ++ t, l')
          (threads, ln') = L.foldr aux ("", ln) rggs
      in (erlTuple [show ln', erlTuple ["par", "[" ++ threads ++ "]"]], 1 + ln')
    Arb p branch ->
      let aux = \(rg, g) -> \(t, l) ->
            let (t', l') = rgc2erl l rg in
              case (t', t) of
                ("", _)  -> (t, l)
                (_, "")  -> (erlList $ erlTuple [erlList t', guard2erl g], l')
                (_ , _)  -> ((erlList $ erlTuple [erlList t', guard2erl g]) ++ " ++ " ++ t, l')
          (branches, ln') = L.foldr aux ("", ln) branch
      in (erlTuple [show ln', erlTuple ["bra", erlAtom "ptp_" p, erlList branches]], 1 + ln')
    Qes rggs ->
      let aux = \rg -> \(t,l) ->
            let (t',l') = rgc2erl l rg in
            case (t,t') of
              ("","") -> ("", l')
              ("", _) -> (t', l')
              (_ , _) -> (t' ++ sep ++ t, l')
          (seq, ln') = L.foldr aux ("", ln) rggs
      in (erlList seq, ln') 
    Per p rgg g ->
      let (body, ln') = rgc2erl ln rgg
      in (erlTuple [show ln', erlTuple ["rec", erlAtom "ptp_" p, erlTuple [erlList body, guard2erl g]]], 1 + ln')

