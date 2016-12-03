module BranchingProperty where

import CFSM
import TS
import Data.List as L
import Data.Set as S
import Data.Map.Strict as M
import Data.Foldable as F
import Data.Maybe
import Dependency
--import Control.Concurrent
--import Control.Exception
--import Data.Ord
--import Data.Tree as T
--import PartialOrderReduction
--import System.IO.Unsafe
--import Debug.Trace

type MapActions = Map Ptp (Set Action)
type EvtPath = [[(Configuration, KEvent, Configuration)]]
type MapPaths = Map Configuration EvtPath


isCommuting :: TSb -> Configuration -> KEvent -> KEvent -> Bool
isCommuting ts@(_, _, _, trans) n e1 e2 = F.or $ S.map (\x -> S.member (n1,e2,x) trans && S.member (n2,e1,x) trans) inter
  where n1 = succConf ts n e1
        n2 = succConf ts n e2
        inter = S.intersection (succConfs ts n1) (succConfs ts n2)
            

--
-- Build the (non transitive version) of <-relation on nodes of TS
--
buildLessRel :: TSb -> Set (Configuration, Configuration)
buildLessRel ts@(nodes, initnode, _, _) =
    helper (S.singleton initnode) S.empty (S.singleton initnode)
    where
      helper border acc visited = 
          let newpairs = S.fold S.union S.empty $
                         S.map (\n -> (S.map (\x -> (n,x))
                                       (S.filter (\y -> not $ S.member y visited) (succConfs ts n)))) border
              newnodes = S.map snd newpairs
          in if S.null border || (visited == nodes)
             then acc
             else helper newnodes (S.union acc newpairs) (S.union newnodes visited)
              
 
fire :: TSb -> Configuration -> KEvent -> Bool
fire (_, _, _, tstrans) n e = not $ S.null $ S.filter (\(x,y,_) -> x==n && y == e) tstrans

--
-- Returns true if a n is a "last node" that fires e1 and e2
--
isLastNodeRel :: TSb -> Set (Configuration, Configuration) -> Configuration -> KEvent -> KEvent -> Bool
isLastNodeRel ts rel n e1 e2 = (F.and $ S.map checkNode allsucc)
  where allsucc = succConfs ts n
        checkNode n' = not ((fire ts n' e1)  && (fire ts n' e2) && (S.member (n,n') rel))


checkBranchingProperty :: System -> TSb -> [Cause Configuration KEvent]
checkBranchingProperty sys ts@(nodes, _, _, _) = checkNodes sys ts (buildLessRel ts) (S.toList nodes)

--
-- Check the Branching property on all nodes and branches in TSb
--
checkNodes :: System -> TSb -> Set (Configuration, Configuration) -> [Configuration] -> [Cause Configuration KEvent]
checkNodes mysys@(_,ptps) ts lessRelation nodes = L.concat $ L.map checkNode nodes
   where
       checkNode n = checkBranch [] n (pairs $ S.toList $ (succEvents ts n))
       pairs xs = L.nubBy eqTest [(x,y) | x <- xs, y <- xs, x /= y]
       eqTest (x,y) (x',y') = ((x == y') && (y == x')) || ((x == x') && (y == y'))
       checkBranch res _ []             = res
       checkBranch res n ((e1, e2):evs) = if (independent e1 e2) || (isCommuting ts n e1 e2) || (not (isLastNodeRel ts lessRelation n e1 e2))
                                          then checkBranch res n evs
                                          else checkBranch (res ++ analysis) n evs
         where oneSender   = uniqueSender mysys ts n e1 map1 e2 map2
               noRace      = checkReceivers (sender e1) mysys ts map1 map2 rng n e1 e2 -- Dependency relations
               chAwareness = (checkParticipants ts rng n e1 map1 e2 map2) -- Good Choice
               msg1        = if oneSender   then "" else "Not unique sender"
               msg2        = if noRace      then "" else "There is a race"
               msg3        = if chAwareness then "" else "No choice awareness"
               analysis    = if oneSender && noRace && chAwareness
                             then []
                             else [Rp n e1 e2 (msg1 ++ " " ++ msg2 ++ " " ++ msg3)]
               succ1       = succConf ts n e1
               succ2       = succConf ts n e2
               rng         = L.map snd (M.assocs ptps)
               map1        = mapActions mysys ts succ1 e1
               map2        = mapActions mysys ts succ2 e2


--
-- Computes all the first action of all machines from a given node
--
mapActions :: System -> TSb -> Configuration -> KEvent -> MapActions
mapActions mysys@(_,ptps) ts n' e = M.fromList $ L.map (\x -> (x, helper x)) ids
  where helper m = case (project e m) of -- n' is the target of e here
          Just a -> S.singleton a
          Nothing -> firstActions ts n' m (possibleActions mysys m n')
        ids = L.map snd (M.assocs ptps)


--
-- PRE: 
-- POST: true iff there is a unique sender for e1 and e2
--
uniqueSender :: System -> TSb -> Configuration -> KEvent -> MapActions -> KEvent -> MapActions -> Bool
uniqueSender (_,ptps) _ _ _ map1 _ map2 =
  let rng     = L.map snd (M.assocs ptps)
      senders = L.map 
                (
                  \m -> let lt1 = map1 ! m
                            lt2 = map2 ! m
                         in
                          if compatibleActions lt1 lt2
                          then existSend $ S.union lt1 lt2
                          else False
                ) rng
  in checkSenders 0 senders
  where checkSenders num (x:xs) = checkSenders (if x then num+1 else num) xs
        checkSenders i [] = if i == 1
                            then True
                            else False --error $ "No unique sender at node: "++(show n) ++ " e1: "++(show e1)++" e2: "++(show e2) ++ " ("++(show i)++" senders)"


--
-- Check condition (c) of the Branching Property
--
checkReceivers :: Ptp -> System -> TSb -> MapActions -> MapActions -> [Ptp] -> Configuration -> KEvent -> KEvent -> Bool
checkReceivers sbj sys ts map1 map2 (p:ps) n e1 e2 =
  if (checkReceiver p n e1 e2) 
  then (checkReceivers sbj sys ts map1 map2 ps n e1 e2)
  else False --error $ ("[RCV-race-condition] Machine: "++(show p)++" Node: "++(show n)++" e1: "++(show e1)++" e2: "++(show e2))
  where
    succ1 = succConf ts n e1
    succ2 = succConf ts n e2
    checkReceiver m _ e e' = 
          let lt1 = map1 ! m
              lt2 = map2 ! m
          in if (not $ existSend $ S.union lt1 lt2) && (compatibleActions lt1 lt2) && (not $ sameSenders lt1 lt2)
             then
               (checkRaceInChoice sbj p ts succ1 e lt1 lt2)
               &&
               (checkRaceInChoice sbj p ts succ2 e' lt2 lt1)
             else True
checkReceivers _ _ _ _ _ [] _ _ _ = True

checkRaceInChoice :: Ptp -> Ptp -> TSb -> Configuration -> KEvent -> Set Action -> Set Action -> Bool  
checkRaceInChoice sbj rcv ts n ei good bad =
  dependencyTS sbj rcv ts n ei (S.map action2interaction good) (S.map action2interaction bad)

--
-- Check condition (a) of the Branching Property
--
checkParticipants :: TSb -> [Ptp] -> Configuration -> KEvent -> MapActions -> KEvent -> MapActions -> Bool
checkParticipants ts (p:ps) n e1 map1 e2 map2 =
    if (checkParticipant p n e1 e2) 
    then (checkParticipants ts ps n e1 map1 e2 map2)
    else False --error $ ("Machine: " ++ (show p) ++ " Node: " ++ (show n) ++ "\n\te1: " ++ (show e1) ++ "\n\te2: " ++ (show e2))
    where
      succ1 = succConf ts n e1
      succ2 = succConf ts n e2
      checkParticipant m n' e e' = 
          let lt1 = map1 ! m
              lt2 = map2 ! m
          in 
           if (compatibleActions lt1 lt2)
           then True
           else 
             goodMergeNode ts m n' succ1 e succ2 e'
checkParticipants _ [] _ _ _ _ _  = True
       
        
compatibleActions :: Set Action -> Set Action -> Bool
compatibleActions set1 set2 = 
  (S.null set1 && S.null set2)
  ||
  (
   ((not $ S.null set1) && (not $ S.null set2))
   &&
   (S.null $ S.intersection set1 set2)
  )

sameSenders :: Set Action -> Set Action -> Bool
sameSenders act1 act2 = (snds act1) == (snds act2)
  where snds acts = S.map (\(_,(s,_),_) -> s) acts



goodMergeNode :: TSb -> Ptp -> Configuration -> Configuration -> KEvent ->  Configuration -> KEvent -> Bool
goodMergeNode ts m n n1 e1 n2 e2 =
  not $ S.null $ S.intersection (setX n1 e1) (setX n2 e2)
  where setX node evt = case project evt m of
          Just _ -> S.singleton n
          Nothing -> epsilonReachable ts m node

--
-- Find all the nodes reachable from another node
--  
epsilonReachable :: TSb -> Ptp -> Configuration -> Set Configuration
epsilonReachable ts m n  = 
  S.fromList $ traverse [n] S.empty [n]
  where
    traverse (n':ns) visited acc =
      if S.member n' visited  
      then traverse ns visited acc
      else let sucnodes = S.toList $
                          S.map (\(_,s) -> s) $ 
                          S.filter (\(l,s) -> (isNothing $ project l m) && (not $ S.member s visited)) $ (S.map (\(_, y, z) -> (y,z)) (deriv n' ts))
           in traverse (ns++sucnodes) (S.insert n' visited) (acc++sucnodes)
    traverse [] _ acc = acc   
         
 

--
-- Find all the nodes reachable from n1 and n2 (closest nodes first)
--
mergeNode :: TSb -> Configuration -> Configuration -> [Configuration]
mergeNode ts n1 n2 = L.map fst intposition
  where reach1 = reachableNode ts n1
        reach2 = reachableNode ts n2
        --
        intsection = S.intersection (S.fromList reach1) (S.fromList reach2)
        --
        intposition = 
          L.sortBy ncompare $ S.toList $
          S.map (\x -> (x, (findPosition x reach1)+(findPosition x reach2))) intsection
        --
        ncompare (_,i) (_,j)
          | i < j = LT
          | i > j = GT
          | otherwise = EQ
        --
        findPosition n xs = case elemIndex n xs of
          Just i -> i
          Nothing -> error "Something went terribly wrong in finding a merging node!"
          
          
