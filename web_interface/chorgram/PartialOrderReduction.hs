--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--

module PartialOrderReduction where

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
  -- F.or $ S.map (\(x,y,z) -> not $ L.elem current $ reachableNode ts z) amp
  not $ F.or $ S.map (\(_,y,_) -> S.member (succConf ts current y) visited) amp


chooseCandidate :: TSb -> Set Configuration -> Configuration -> Set KTrans
chooseCandidate ts visited current = findAmple (ample ts current) alltrans 
  where findAmple (x:xs) allt = if (x==allt
                                   )
                                   || 
                                   (
                                     (checkC1 ts x (S.difference allt x))
                                     &&
                                     (checkC3 ts visited current x)
                                   )
                                then x 
                                else findAmple xs allt
        findAmple [] allt = allt
        alltrans = deriv current ts


--
--
reduce :: TSb -> TSb
reduce ts@(_,initnode,_,_) = (newConfs, initnode, newEvents, newTrans) 
  where newConfs  = S.fold S.union S.empty $ S.map (\((x,bx),_,(z,bz)) -> S.insert (x,bx) (S.singleton (z,bz))) newTrans
        newEvents = S.fold S.union S.empty $ S.map (\(_,y,_) ->  S.singleton y) newTrans
        newTrans  = S.fromList $ rebuild [initnode] S.empty []
        --
        rebuild (current:xs) visited acc
          | S.member current visited = rebuild xs visited acc
          | otherwise = let nextrans = S.toList $ chooseCandidate ts visited current
                            nextstates = L.map (\(_,_,z) -> z) nextrans
                        in rebuild (xs++nextstates) (S.insert current visited) (acc++nextrans)
        rebuild [] _ acc = acc
