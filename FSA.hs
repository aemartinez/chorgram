--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- Stuff about dealing CFSMs as FSA
--


module FSA where

import SystemParser
import Misc
import CFSM
import Data.Set as S
import Data.List as L

pTransitionsRemoval :: CFSM -> (Action -> Bool) -> CFSM
-- removes the transitions whose action satisfies the predicate
pTransitionsRemoval m@(states, q0, acts, trxs) lpred = (states, q0, acts, trxs')
  where trxs'   = S.difference (L.foldl S.union otrxs (L.map inherit pairs)) (S.filter (\(_,l,_) -> isTau l) trxs)
        otrxs   = S.filter (\(_,l,_) -> not(lpred l)) trxs
        pairs   = [ (q,q') | q <- S.toList states, q' <- S.toList states, not(q==q'), S.member q' (pClosure m isTau q)]
        inherit = \(q1,q2) -> S.map (\(_,l,q') -> (q1,l,q')) (S.intersection otrxs (goutgoing m q2))
  

eqClassOf :: (Ord a) => a -> [Set a] -> Set a
-- returns the first set in 'classes' containing 'state'
eqClassOf state classes =
  case classes of
    []          -> S.empty
    qs:classes' -> if (S.member state qs) then qs else (eqClassOf state classes')

flatSet :: Set State -> State
-- turns a set of states into a state
flatSet states = S.foldr ( \q q' -> (q ++ "__" ++ q') ) "" states 

flat :: Agraph (Set State) Action -> CFSM
flat (states, q0, labels, trxs) = (S.map flatSet states, flatSet q0, labels, S.map (\(q,l,q') -> (flatSet q, l, flatSet q')) trxs)

-- PRE: the input has to be a deterministic machine
-- POST: return the minimal machine equivalent to the input machine
determinise :: CFSM -> CFSM
-- the subset construction on CFSMs
determinise m = flat (S.fromList states, q0_, acts, trs')
  where
    q0_ = S.singleton q0
    m'@(vs,q0,acts, trxs) = pTransitionsRemoval m isTau
--     aux (S.singleton q0_) (S.singleton q0_, q0_, acts, S.emtpy)
    aux todo done current =
      if S.null todo
      then current
      else let s = S.elemAt 0 todo
           in if S.member s done
              then aux (S.delete s todo) done (states, transitions)
              else let done'    = S.insert s done
                       actMap qqs amap =
                         if S.null qqs
                         then amap
                         else
                           let q = S.elemAt 0 qqs
                               mapIns (_,act,q') = if M.member act amap
                                                   then M.insert act (S.insert q' amap!q)
                                                   else M.insert act (S.singleton q')
                               amap' = S.map mapIns (goutgoing m q)
                           in actMap (S.delete q qqs) amap'
                       rset = actMap s M.empty
                       todo' = S.map (\qq -> S.insert qq todo) M.values rset
                       
                                            
                       
                      in aux (S.delete (S.elemAt 0 todo) todo') done' current'

        
-- PRE: the input has to be a deterministic machine
-- POST: return the minimal machine equivalent to the input machine
minimise :: CFSM -> CFSM
-- Variant of the partition refinement algorithm were all states are final
minimise m =
  if (S.size vs <= 1)
  then m
  else flat (S.fromList states, q0, acts, trs')
  where m'@(vs,q0',acts, trxs) = pTransitionsRemoval m isTau
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

-- -- minimisation testing
-- main :: IO ()
-- main =  do progargs <- getArgs
--            let sourcefile = last progargs
--            cfsmFile <- readFile sourcefile
--            let m = parseFSA cfsmFile
--            let m'@(vs,q0',acts, trxs) = pTransitionsRemoval m isTau
--            let states = getPartitions [vs]
--            let q0 = eqClassOf q0' states
--            let trs' = S.map (\(q,l,q') -> ((eqClassOf q states), l, (eqClassOf q' states)) ) trxs
--            let getPartitions currentStates = do
--                  let addState state classes =
--                        case classes of
--                          []          -> [S.singleton state]
--                          qs:classes' -> if (equiv (S.elemAt 0 qs) state)
--                                         then (S.insert state qs):classes'
--                                         else qs:(addState state classes')
--                  let equiv q q' = (transFrom q) == (transFrom q')
--                  let transFrom = \q -> S.map (\(_,l,t) -> (l,(eqClassOf t currentStates))) (goutgoing m' q)
--                  let refineStep qs = if (S.size qs <= 1)
--                                      then [qs]
--                                      else addState (S.elemAt 0 qs) (refineStep (S.delete (S.elemAt 0 qs) qs))
--                  let nextStates = flatten (L.map refineStep currentStates)
--                  if currentStates == nextStates
--                    then do putStrLn currentStates
--                    else do putStrLn currentStates
--                            getPartitions nextStates
