--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- Stuff about dealing CFSMs as FSA
--
-- TODO: the determinisation and minisation of CFSMs looses the
--       information on the original states. Tracking states is
--       probably necessary for future development.  Making the
--       functions to return a map could fix this.
--


module FSA where

import Misc
import CFSM
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

cfsm2graph :: CFSM -> Graph Int Action
cfsm2graph  (states, q0, ls, trxs) =
  let
    dict = M.fromList [(q, S.findIndex q states) | q <- S.toList states]
  in
    (S.fromList $ M.elems dict, dict!q0, ls, S.map (\(q,l,q') -> (dict!q, l, dict!q')) trxs)

graph2cfsm :: Graph (Set Int) Action -> CFSM
graph2cfsm (states, q0, ls, trxs) =
  (S.map show states, show q0, ls, S.map (\(q,l,q') -> (show q, l, show q')) trxs)

determinise :: CFSM -> CFSM
determinise m =
  graph2cfsm $ determinisation (cfsm2graph m) isCommunication


determinisation :: Ord vertex => Ord label => Graph vertex label -> (label -> Bool) -> Graph (Set vertex) label
determinisation m pred =
  -- the powerset construction on any graph
  -- PRE: 'pred' determines which are the observable transitions (eg non-epsilon)
  -- POST: return the minimal machine equivalent to the input machine
  (states, q0_, acts, trxs)
  where
    m'@(_, q0, acts, _) = pRemoval m (not . pred)
    q0_ = S.singleton q0
    (states, trxs) = aux (S.singleton q0_) S.empty (S.singleton q0_, S.empty)
    aux todo done current =
      if S.null todo
      then current
      else
        let
          s = S.elemAt 0 todo
          mapIns amap_ trxs_ =
            case trxs_ of
              []                  -> amap_
              (_, act, q'):trsx_' ->
                let
                  x =
                    if M.member act amap_
                    then S.insert q' (amap_!act)
                    else S.singleton q'
                in
                  mapIns (M.insert act x amap_) trsx_'
        in
          if S.member s done
          then aux (S.delete s todo) done current
          else
            let
              actMap qqs amap =
                if S.null qqs
                then amap
                else
                  let
                    state = S.elemAt 0 qqs
                    amap' = mapIns amap (S.toList $ fanOut m' state)
                  in
                    actMap (S.delete state qqs) amap'
              rset  = actMap s M.empty
              strxs = S.map ( \(_, act, _) -> (s, act, rset!act)) (S.unions $ S.map (fanOut m') s)
              (newqs, newtrxs) = (S.fromList $ M.elems rset, strxs)
              current' = (S.union newqs (fst current), S.union newtrxs (snd current))
            in
              aux (S.delete s (S.union todo newqs)) (S.insert s done) current'

minimisation :: Ord label => Graph Int label -> (label -> Bool) -> Graph Int label
minimisation m pred =
  -- Variant of the partition refinement algorithm were all states are final
  -- PRE: 'pred' determines which are the observable transitions (eg non-epsilon)
  -- POST: return the minimal language-equivalent graph
  states2int (S.fromList states, q0, acts, trs')
  where m'@(vs,q0',acts, trxs) = determinisation m pred
        -- initially all states are equivalent
        states = getPartitions [vs]
        -- the initial state is the class containing the initial state of m
        q0 = eqClassOf q0' states
        trs' = S.map (\(q,l,q') -> ((eqClassOf q states), l, (eqClassOf q' states)) ) trxs
        getPartitions currentStates =
          let
            addState state classes =
              case classes of
                []          -> [S.singleton state]
                qs:classes' ->
                  if (equiv (S.elemAt 0 qs) state)
                  then (S.insert state qs):classes'
                  else qs:(addState state classes')
            equiv q q' = (transFrom q) == (transFrom q')
            transFrom = \q -> S.map (\(_, l, t) -> (l, (eqClassOf t currentStates))) (goutgoing m' q)
            refineStep qs =
              if (S.size qs <= 1)
              then [qs]
              else addState (S.elemAt 0 qs) (refineStep (S.delete (S.elemAt 0 qs) qs)) 
            nextStates = flatten (L.map refineStep currentStates)
          in
            if currentStates == nextStates
            then currentStates
            else getPartitions nextStates

minimise :: CFSM -> CFSM
minimise m =
  aux $ minimisation (cfsm2graph m) isCommunication
  where
    aux (states, q0, ls, trxs) =
      (S.map show states, show q0, ls, S.map (\(q,l,q') -> (show q, l, show q')) trxs)



