--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- Stuff about FSA
--
-- TODO: take silent transitions into account!!!
--


module FSA where

import Misc
import Data.Set as S
import Data.List as L

flatten :: [[a]] -> [a]
-- nomen omen
flatten ls =
  case ls of
    []    -> []
    l:lls -> [e | e <-l] ++ (flatten lls)

eqClassOf :: (Ord a) => a -> [Set a] -> Set a
-- eqClassOf returns the first set in 'classes' containing 'state'
eqClassOf state classes =
  case classes of
    []          -> S.empty
    qs:classes' -> if (S.member state qs) then qs else (eqClassOf state classes')

minimise :: (Ord vertex,Ord label) => Agraph vertex label -> Agraph (Set(vertex)) label
-- Variant of the partition refinement algorithm were all states are final
minimise g@(vs,v,labels, trs) =
  if (S.size vs <= 1)
  then (S.map S.singleton vs, S.singleton v, labels, S.map (\(q,l,q') -> (S.singleton q, l, S.singleton q')) trs)
  else (S.fromList states, q0, labels, trs')
  where states = getPartitions [vs]  -- initially all states are equivalent
        q0 = eqClassOf v states   -- the initial state is the class containing the initial state of g
        trs' = S.map (\(q,l,q') -> ((eqClassOf q states), l, (eqClassOf q' states)) ) trs
        getPartitions currentStates =
          let addState state classes =
                case classes of
                  []          -> [S.singleton state]
                  qs:classes' -> if (equiv (S.elemAt 0 qs) state)
                                 then (S.insert state qs):classes'
                                 else qs:(addState state classes')
              equiv q q' = (transFrom q) == (transFrom q')
              transFrom = \q -> S.map (\(_,l,t) -> (l,(eqClassOf t currentStates))) (goutgoing g q)
              refineStep qs = if (S.size qs <= 1)
                              then [qs]
                              else addState (S.elemAt 0 qs) (refineStep (S.delete (S.elemAt 0 qs) qs)) 
              nextStates = flatten (L.map refineStep currentStates)
          in if currentStates == nextStates
             then currentStates
             else getPartitions nextStates
               
