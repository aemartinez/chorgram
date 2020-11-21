--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- Stuff about dealing CFSMs as FSA
--


module FSA where

import Misc
import CFSM
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

predTrxRemoval :: CFSM -> (Action -> Bool) -> CFSM
-- removes the transitions whose action satisfies the predicate lpred
predTrxRemoval m@(states, q0, acts, trxs) lpred = (states, q0, acts, trxs')
  where trxs'   = S.difference (L.foldl S.union otrxs (L.map inherit pairs)) (S.filter (\(_,l,_) -> lpred l) trxs)
        otrxs   = S.filter (\(_, l, _) -> not(lpred l)) trxs
        pairs   = [ (q,q') | q <- S.toList states, q' <- S.toList states, not(q==q'), S.member q' (pClosure m lpred q) ]
        inherit = \(q1,q2) -> S.map (\(_, l, q') -> (q1, l, q')) (S.intersection otrxs (goutgoing m q2))
  

eqClassOf :: (Ord a) => a -> [Set a] -> Set a
-- returns the first set in 'classes' containing 'state'
eqClassOf state classes =
  case classes of
    []          -> S.empty
    qs:classes' -> if (S.member state qs) then qs else (eqClassOf state classes')

flatSet :: Set State -> State
-- turns a set of states into a state
flatSet states = S.foldr ( \q q' -> (q ++ "__" ++ q') ) "" states 

flat :: Graph (Set State) Action -> CFSM
flat (states, q0, labels, trxs) =
  (S.map flatSet states, flatSet q0, labels, S.map (\(q, l, q') -> (flatSet q, l, flatSet q')) trxs)

  
determinise :: CFSM -> CFSM
-- the powerset construction on CFSMs
-- PRE: the input has to be a tau-less machine
-- POST: return the minimal machine equivalent to the input machine
determinise m = flat (states, q0_, acts, trxs)
  where
    m'@(_, q0, acts, _) = pRemoval m (\x -> not (isCommunication x))
    q0_ = S.singleton q0
    (states, trxs) = aux (S.singleton q0_) S.empty (S.singleton q0_, S.empty)
    aux todo done current =
      if S.null todo
      then current
      else let s = S.elemAt 0 todo
               mapIns :: Map Action (Set State) -> [LTrans] -> Map Action (Set State)
               mapIns amap_ trxs_ =
                 case trxs_ of
                   []                  -> amap_
                   (_, act, q'):trsx_' ->
                     let x = if M.member act amap_
                             then S.insert q' (amap_!act)
                             else S.singleton q'
                     in mapIns (M.insert act x amap_) trsx_'
           in if S.member s done
              then aux (S.delete s todo) done current
              else
                let actMap =
                      \qqs amap ->
                        if S.null qqs
                        then amap
                        else
                          let state = S.elemAt 0 qqs
                              amap' = mapIns amap (S.toList $ step m' state)
                          in actMap (S.delete state qqs) amap'
                    rset  = actMap s M.empty
                    strxs = S.map (\(act, _) -> (s, act, rset!act)) (S.unions (L.map (\q -> (succs m' q)) (S.toList s)))
                    (newqs, newtrxs) = (S.fromList $ M.elems rset, strxs)
                in aux (S.delete s (S.union todo newqs)) (S.insert s done) (S.union newqs (fst current), S.union newtrxs (snd current))


minimise :: CFSM -> CFSM
-- Variant of the partition refinement algorithm were all states are final
-- PRE: the input has to be a deterministic machine
-- POST: return the minimal machine equivalent to the input machine
minimise m = flat (S.fromList states, q0, acts, trs')
  where m'@(vs,q0',acts, trxs) = determinise m
        -- initially all states are equivalent
        states = getPartitions [vs]
        -- the initial state is the class containing the initial state of g
        q0 = eqClassOf q0' states
        trs' = S.map (\(q,l,q') -> ((eqClassOf q states), l, (eqClassOf q' states)) ) trxs
        getPartitions currentStates =
          let addState state classes =
                case classes of
                  []          -> [S.singleton state]
                  qs:classes' -> if (equiv (S.elemAt 0 qs) state)
                                 then (S.insert state qs):classes'
                                 else qs:(addState state classes')
              equiv q q' = (transFrom q) == (transFrom q')
              transFrom = \q -> S.map (\(_, l, t) -> (l, (eqClassOf t currentStates))) (goutgoing m' q)
              refineStep qs = if (S.size qs <= 1)
                              then [qs]
                              else addState (S.elemAt 0 qs) (refineStep (S.delete (S.elemAt 0 qs) qs)) 
              nextStates = flatten (L.map refineStep currentStates)
          in if currentStates == nextStates
             then currentStates
             else getPartitions nextStates



