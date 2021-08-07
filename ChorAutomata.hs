--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module contains operations on c-automata.
--

module ChorAutomata where

import CFSM (Channel)
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import Misc
import DotStuff

data Interaction =
  Comm Channel Message
  | Tau
  deriving (Ord, Eq, Show)

type CAutomaton = Graph Int Interaction

trxClosure :: (Interaction -> Bool) -> CAutomaton -> CAutomaton
trxClosure cond (states, q0, ls, trxs) =
  let
    closure q =
      let
        tgt = \(_, _, t) -> t
        step = S.map tgt (S.filter (\(q', l, _) -> q' == q && (cond l)) trxs)
      in
        S.union (S.singleton q) (S.foldr S.union step (S.map closure step))
    states' = S.map closure states
    trxsCond = M.fromList [(q, S.filter (\(s,l,t) -> s==q && not(cond l)) trxs) | q <- S.toList states]
    aux cl (q,l,q') = (cl, l, S.findIndex (closure q') states')
    newTr qs = S.map (aux (S.findIndex qs states')) (S.unions $ S.map (\q -> trxsCond!q) qs)
    trxs' = S.unions $ S.map newTr states'
  in
    (S.map (\q -> S.findIndex q states') states', (S.findIndex (closure q0) states'), ls, trxs')

interaction2string :: Interaction -> String
interaction2string Tau = "tau"
interaction2string (Comm (s,r) l) = (show s) ++ " -> " ++ (show r) ++ ": " ++ (show l)

ca2dot :: CAutomaton -> String -> Map String String -> String
ca2dot ( states, q0, _, trxs ) name flines =
  "digraph " ++ name ++ " {\n\tlabel=\"" ++ name ++ "\"\n" ++ pstates ++ ptx ++ "\n}"
  where pstates =
          "\t" ++ (show q0) ++
          "\t[style=" ++ (flines!q0style) ++
          ", color=" ++ (flines!q0col) ++ "]\n" ++
          S.foldr (++) ""
            (S.map (\x ->
                      "\t" ++ (show x) ++
                         "\t[label = \"" ++
                          (show x) ++
                         "\"];\n"
                   )
              states
            )
        ptx =
          S.foldr (++) "" (
            S.map (\( s, int, t ) -> 
                  "\t" ++ (show s)
                  ++ " -> " ++
                  (show t)
                  ++
                  "\t[label = \"" ++ (rmChar '\"' $ interaction2string int) ++ "\"];\n"
            )
            trxs
          )
