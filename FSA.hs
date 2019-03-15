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
import Data.Map.Strict as M

-------------------------------------------------------
-- for testing purposes
sys = [(S.fromList ["q0","q00*qe0","qe","qe*qe0","qe0","qe0*q00","qe0*qe","qe00","qe01","qe0210","qe0220","qe022110","qe022111","qe022112","qe022120","qe022130","qe022131","qe02213210","qe02213220","qe02213221"],"q0",S.fromList [(("A","A"),Tau,"tau"),(("A","A"),Break,"break"),(("A","B"),Send,"authReq"),(("A","B"),Send,"authWithdrawal"),(("A","B"),Send,"getBalance"),(("A","C"),Send,"authFail"),(("A","C"),Send,"balance"),(("A","C"),Send,"bye"),(("A","C"),Send,"money"),(("B","A"),Receive,"allow"),(("B","A"),Receive,"balance"),(("B","A"),Receive,"denied"),(("B","A"),Receive,"deny"),(("B","A"),Receive,"granted"),(("C","A"),Receive,"*<0"),(("C","A"),Receive,">*0"),(("C","A"),Receive,"auth"),(("C","A"),Receive,"checkBalance"),(("C","A"),Receive,"quit"),(("C","A"),Receive,"withdraw")],
         S.fromList [("q0",(("C","A"),Receive,"auth"),"qe00"),("q00*qe0",(("A","A"),Tau,"tau"),"q0"),("qe*qe0",(("A","A"),Tau,"tau"),"qe"),("qe0",(("A","A"),Tau,"tau"),"qe0*q00"),("qe0",(("A","A"),Tau,"tau"),"qe0*qe"),("qe0",(("C","A"),Receive,"*<0"),"q00*qe0"),("qe0",(("C","A"),Receive,">*0"),"qe*qe0"),("qe0*q00",(("C","A"),Receive,"*<0"),"q0"),("qe0*qe",(("C","A"),Receive,">*0"),"qe"),("qe00",(("A","B"),Send,"authReq"),"qe01"),("qe01",(("B","A"),Receive,"denied"),"qe0210"),("qe01",(("B","A"),Receive,"granted"),"qe0220"),("qe0210",(("A","C"),Send,"authFail"),"qe0"),("qe0220",(("C","A"),Receive,"checkBalance"),"qe022110"),("qe0220",(("C","A"),Receive,"quit"),"qe022120"),("qe0220",(("C","A"),Receive,"withdraw"),"qe022130"),("qe022110",(("A","B"),Send,"getBalance"),"qe022111"),("qe022111",(("B","A"),Receive,"balance"),"qe022112"),("qe022112",(("A","C"),Send,"balance"),"qe0"),("qe022120",(("A","A"),Break,"break"),"qe0"),("qe022130",(("A","B"),Send,"authWithdrawal"),"qe022131"),("qe022131",(("B","A"),Receive,"allow"),"qe02213210"),("qe022131",(("B","A"),Receive,"deny"),"qe02213220"),("qe02213210",(("A","C"),Send,"money"),"qe0"),("qe02213220",(("A","C"),Send,"bye"),"qe02213221"),("qe02213221",(("A","A"),Break,"break"),"qe0")]),
       (S.fromList ["q0","q00*qe0","qe","qe*qe0","qe0","qe0*q00","qe0*qe","qe00","qe01","qe0210","qe0220","qe022110","qe022111","qe022112","qe022120","qe022130","qe022131","qe02213210","qe02213220","qe02213221"],"q0",S.fromList [(("A","B"),Receive,"authReq"),(("A","B"),Receive,"authWithdrawal"),(("A","B"),Receive,"getBalance"),(("B","A"),Send,"allow"),(("B","A"),Send,"balance"),(("B","A"),Send,"denied"),(("B","A"),Send,"deny"),(("B","A"),Send,"granted"),(("B","B"),Tau,"tau"),(("B","B"),Break,"break"),(("C","B"),Receive,"*<0"),(("C","B"),Receive,">*0")],
         S.fromList [("q0",(("B","B"),Tau,"tau"),"qe00"),("q00*qe0",(("C","B"),Receive,"*<0"),"q0"),("qe*qe0",(("C","B"),Receive,">*0"),"qe"),("qe0",(("B","B"),Tau,"tau"),"q00*qe0"),("qe0",(("B","B"),Tau,"tau"),"qe*qe0"),("qe0",(("C","B"),Receive,"*<0"),"qe0*q00"),("qe0",(("C","B"),Receive,">*0"),"qe0*qe"),("qe0*q00",(("B","B"),Tau,"tau"),"q0"),("qe0*qe",(("B","B"),Tau,"tau"),"qe"),("qe00",(("A","B"),Receive,"authReq"),"qe01"),("qe01",(("B","A"),Send,"denied"),"qe0210"),("qe01",(("B","A"),Send,"granted"),"qe0220"),("qe0210",(("B","B"),Tau,"tau"),"qe0"),("qe0220",(("B","B"),Tau,"tau"),"qe022110"),("qe0220",(("B","B"),Tau,"tau"),"qe022120"),("qe0220",(("B","B"),Tau,"tau"),"qe022130"),("qe022110",(("A","B"),Receive,"getBalance"),"qe022111"),("qe022111",(("B","A"),Send,"balance"),"qe022112"),("qe022112",(("B","B"),Tau,"tau"),"qe0"),("qe022120",(("B","B"),Break,"break"),"qe0"),("qe022130",(("A","B"),Receive,"authWithdrawal"),"qe022131"),("qe022131",(("B","A"),Send,"allow"),"qe02213210"),("qe022131",(("B","A"),Send,"deny"),"qe02213220"),("qe02213210",(("B","B"),Tau,"tau"),"qe0"),("qe02213220",(("B","B"),Tau,"tau"),"qe02213221"),("qe02213221",(("B","B"),Break,"break"),"qe0")]),
       (S.fromList ["q0","q00*qe0","qe","qe*qe0","qe0","qe0*q00","qe0*qe","qe00","qe01","qe0210","qe0220","qe022110","qe022111","qe022112","qe022120","qe022130","qe022131","qe02213210","qe02213220","qe02213221"],"q0",S.fromList [(("A","C"),Receive,"authFail"),(("A","C"),Receive,"balance"),(("A","C"),Receive,"bye"),(("A","C"),Receive,"money"),(("C","A"),Send,"*<0"),(("C","A"),Send,">*0"),(("C","A"),Send,"auth"),(("C","A"),Send,"checkBalance"),(("C","A"),Send,"quit"),(("C","A"),Send,"withdraw"),(("C","B"),Send,"*<0"),(("C","B"),Send,">*0"),(("C","C"),Tau,"tau"),(("C","C"),Break,"break")],
         S.fromList [("q0",(("C","A"),Send,"auth"),"qe00"),("q00*qe0",(("C","B"),Send,"*<0"),"q0"),("qe*qe0",(("C","B"),Send,">*0"),"qe"),("qe0",(("C","A"),Send,"*<0"),"q00*qe0"),("qe0",(("C","A"),Send,">*0"),"qe*qe0"),("qe0",(("C","B"),Send,"*<0"),"qe0*q00"),("qe0",(("C","B"),Send,">*0"),"qe0*qe"),("qe0*q00",(("C","A"),Send,"*<0"),"q0"),("qe0*qe",(("C","A"),Send,">*0"),"qe"),("qe00",(("C","C"),Tau,"tau"),"qe01"),("qe01",(("C","C"),Tau,"tau"),"qe0210"),("qe01",(("C","C"),Tau,"tau"),"qe0220"),("qe0210",(("A","C"),Receive,"authFail"),"qe0"),("qe0220",(("C","A"),Send,"checkBalance"),"qe022110"),("qe0220",(("C","A"),Send,"quit"),"qe022120"),("qe0220",(("C","A"),Send,"withdraw"),"qe022130"),("qe022110",(("C","C"),Tau,"tau"),"qe022111"),("qe022111",(("C","C"),Tau,"tau"),"qe022112"),("qe022112",(("A","C"),Receive,"balance"),"qe0"),("qe022120",(("C","C"),Break,"break"),"qe0"),("qe022130",(("C","C"),Tau,"tau"),"qe022131"),("qe022131",(("C","C"),Tau,"tau"),"qe02213210"),("qe022131",(("C","C"),Tau,"tau"),"qe02213220"),("qe02213210",(("A","C"),Receive,"money"),"qe0"),("qe02213220",(("A","C"),Receive,"bye"),"qe02213221"),("qe02213221",(("C","C"),Break,"break"),"qe0")])
      ]
-- remove up to here
-------------------------------------------------------

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
-- the subset construction on CFSMs
-- PRE: the input has to be a tau-less machine
-- POST: return the minimal machine equivalent to the input machine
determinise m = flat (states, q0_, acts, trxs)
  where
    m'@(_, q0, acts, _) = pRemoval m isTau
    q0_ = S.singleton q0
    (states, trxs) = aux (S.singleton q0_) S.empty (S.singleton q0_, S.empty)
    aux todo done current =
      if S.null todo
      then current
      else let s = S.elemAt 0 todo
           in if S.member s done
              then aux (S.delete s todo) done current
              else
                let actMap =
                      \qqs amap ->
                        if S.null qqs
                        then amap
                        else
                          let q = S.elemAt 0 qqs
                              mapIns :: Map Action (Set State) -> [LTrans] -> Map Action (Set State)
                              mapIns amap_ trxs_ =
                                case trxs_ of
                                  []                  -> amap_
                                  (_, act, q'):trsx_' ->
                                    let x = if M.member act amap_
                                            then S.insert q' (amap_!act)
                                            else S.singleton q'
                                    in mapIns (M.insert act x amap_) trsx_'
                              amap' = mapIns amap (S.toList $ step m' q)
                          in actMap (S.delete q qqs) amap'
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
              transFrom = \q -> S.map (\(_,l,t) -> (l,(eqClassOf t currentStates))) (goutgoing m' q)
              refineStep qs = if (S.size qs <= 1)
                              then [qs]
                              else addState (S.elemAt 0 qs) (refineStep (S.delete (S.elemAt 0 qs) qs)) 
              nextStates = flatten (L.map refineStep currentStates)
          in if currentStates == nextStates
             then currentStates
             else getPartitions nextStates


