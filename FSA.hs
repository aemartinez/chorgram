--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- Stuff about dealing CFSMs as FSA
--


module FSA where

import Misc
import CFSM
import Data.Set as S
import Data.List as L


pTransitionsRemoval :: CFSM -> (Action -> Bool) -> CFSM
pTransitionsRemoval m@(states, q0, acts, trxs) lpred = (states, q0, acts, trxs')
  where trxs' = S.difference (L.foldl S.union otrxs (L.map inherit pairs)) (S.filter (\(_,l,_) -> isTau l) trxs)
        otrxs = S.filter (\(_,l,_) -> not(lpred l)) trxs
        pairs = [ (q,q') | q <- S.toList states, q' <- S.toList states, not(q==q'), S.member q' (pClosure m isTau q)]
        inherit (q1,q2) = S.map (\(_,l,q') -> (q1,l,q')) (S.intersection otrxs (goutgoing m q2))
  

eqClassOf :: (Ord a) => a -> [Set a] -> Set a
-- eqClassOf returns the first set in 'classes' containing 'state'
eqClassOf state classes =
  case classes of
    []          -> S.empty
    qs:classes' -> if (S.member state qs) then qs else (eqClassOf state classes')

flatSet :: Set State -> State
flatSet states = S.foldr ( \q q' -> (q ++ "__" ++ q') ) "" states 

flat :: Agraph (Set State) Action -> CFSM
flat (states, q0, labels, trxs) = (S.map flatSet states, flatSet q0, labels, S.map (\(q,l,q') -> (flatSet q, l, flatSet q')) trxs)

-- PRE: gr represents a finite automaton
-- POST: minimise gr is the minimal automaton 
minimise :: CFSM -> CFSM
-- Variant of the partition refinement algorithm were all states are final
minimise m =
  if (S.size vs <= 1)
  then m
  else flat (S.fromList states, q0, acts, trs')
  where m'@(vs,v,acts, trxs) = pTransitionsRemoval m isTau
        states = getPartitions [vs]  -- initially all states are equivalent
        q0 = eqClassOf v states   -- the initial state is the class containing the initial state of g
        trs' = S.map (\(q,l,q') -> ((eqClassOf q states), l, (eqClassOf q' states)) ) trxs
        getPartitions currentStates =
          let addState state classes =
                case classes of
                  []          -> [S.singleton state]
                  qs:classes' -> if (equiv (S.elemAt 0 qs) state)
                                 then (S.insert state qs):classes'
                                 else qs:(addState state classes')
              equiv q q' = (transFrom q) == (transFrom q')
              transFrom = \q -> S.map (\(_,l,t) -> (l,(eqClassOf t currentStates))) (goutgoing m' q)
              refineStep qs = if (S.size qs <= 1)
                              then [qs]
                              else addState (S.elemAt 0 qs) (refineStep (S.delete (S.elemAt 0 qs) qs)) 
              nextStates = flatten (L.map refineStep currentStates)
          in if currentStates == nextStates
             then currentStates
             else getPartitions nextStates
               
