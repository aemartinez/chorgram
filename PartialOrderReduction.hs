--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio.tuosto@gssi.it>
--

module PartialOrderReduction where

import CFSM
import TS
import Data.Set as S
import Data.List as L
import Data.Foldable as F

--
-- /!\ DO A MORE CLEVER TRAVERSAL HERE?
--
checkC1 :: TSb -> Set KTrans -> Set KTrans -> Bool
checkC1 ts amp others = 
  F.and $ S.map (\(_,e,n) -> check n (S.delete e (S.map (\(_,y,_) -> y) amp)) S.empty) others
  where check node evtample visited = 
          (S.member node visited)
          ||
          (S.null evtample)
          ||
          (
            let next = S.filter (\(_,y,_) -> not $ S.member y evtample) (deriv node ts)
                evts = S.map (\(_,y,_) -> y) next
            in 
             (F.and $ S.map (\x -> (indep x evtample) || S.null evtample) evts)
             &&
             (F.and $ S.map (\(_,y,z) -> check z (S.delete y evtample) (S.insert node visited)) next)
          )
        --
        indep evt set = F.and $ S.map (\x -> independent x evt) set

--
--
checkC3 :: TSb -> Set Configuration -> Configuration -> Set KTrans -> Bool
checkC3 ts visited current amp = 
  --    F.or $ S.map (\(x,y,z) -> not $ L.elem current $ reachableNode ts z) amp
  not $ F.or $ S.map (\(_,y,_) -> S.member (succConf ts current y) visited) amp


chooseCandidate :: TSb -> Set Configuration -> Configuration -> Set KTrans
chooseCandidate ts visited n = findAmple (ample n ts) (deriv n ts)
  where findAmple (x:xs) allt = if (x==allt
                                   )
                                   || 
                                   (
                                     (checkC1 ts x (S.difference allt x))
                                     &&
                                     (checkC3 ts visited n x)
                                   )
                                then x
                                else findAmple xs allt
        findAmple [] allt = allt


ample :: Configuration -> TSb -> [Set KTrans]
ample n ts = if allSelfLoops then [next] else L.map snd $ L.sortBy compareList events
  where next          = deriv n ts
        allSelfLoops  = F.and $ S.map (\(x,_,z) -> x==z) next
        transList     = S.toList $ S.map (\x -> (machines x, S.singleton x)) next
        events        = mygroup (length transList) transList
        dijointPtps        = \ (m1,_) (m2,_) -> not $ S.null (S.intersection m1 m2)
        pairwiseUnion = \ xs -> L.map (\ys ->  L.foldr (\ (m1,e1) (m2,e2) -> (S.union m1 m2, S.union e1 e2)) (S.empty, S.empty) ys) xs
        --
        mygroup :: Int -> [(Set Ptp, Set KTrans)] -> [(Set Ptp, Set KTrans)] 
        mygroup i xs  = let newlist = pairwiseUnion (L.groupBy dijointPtps xs)
                        in if i > (length newlist)
                           then mygroup (length newlist) newlist
                           else xs
        compareList  = \ (_,e1) (_,e2) -> compare (S.size e1) (S.size e2)
--
--
reduce :: TSb -> TSb
reduce ts@(_,n0,_,_) = (newConfs, n0, newEvents, newTrans) 
  where newConfs  = S.fold S.union S.empty $ S.map (\((x,bx),_,(z,bz)) -> S.insert (x,bx) (S.singleton (z,bz))) newTrans
        newEvents = S.fold S.union S.empty $ S.map (\(_,y,_) ->  S.singleton y) newTrans
        newTrans  = S.fromList $ rebuild [n0] S.empty []
        --
        rebuild [] _ acc = acc
        rebuild (n:ns) visited acc
          | S.member n visited = rebuild ns visited acc
          | otherwise =
              let nextrans   = S.toList $ chooseCandidate ts visited n
                  nextstates = L.map (\(_,_,n') -> n') nextrans
              in rebuild (ns ++ nextstates) (S.insert n visited) (acc ++ nextrans)
