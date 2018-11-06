module Dependency where

import CFSM
import TS
import Data.List as L
import Data.Set as S
import Data.Foldable as F
import PartialOrderReduction

--
-- lhd-relation 
--
dependentEvents :: KEvent -> KEvent -> Bool
dependentEvents e1 e2 = dependentInteraction (evt2interaction e1) (evt2interaction e2)

--
-- lhd-relation on interactions
--
dependentInteraction :: Interaction -> Interaction -> Bool
dependentInteraction (s, r, _) (s',r',_) = ((s == s') && (r == r'))  ||  (r == s')


sameMachines :: Interaction -> Interaction -> Bool
sameMachines (s, r, _) (s', r', _) = (s' == s) && (r == r')


dependencyTS :: Ptp -> Ptp -> TSb -> Configuration -> KEvent -> Set Interaction -> Set Interaction -> Bool
dependencyTS s r ts n e lt1 lt2 = explore n [] (if S.member (evt2interaction e) lt1 then [e] else []) lt2
  where explore current visited acc nlt2 = 
          let newtrxs = S.filter (\(_,l,n') -> not $ L.elem (current,l,n') visited) (deriv current (reduce $ reinit ts n))
          in F.and $ S.map (\t -> dispatch t visited acc nlt2) newtrxs
        dispatch t@(_,l,n') visited acc nlt2
          | S.null nlt2 = True
          | not (L.null acc) && (receiver l == s) && (sender l == r) = True
            -- If the receiver (being checked) sends something back the sender, before reaching "lt2", it's
            -- fine since there is a unique selector
          | S.member (evt2interaction l) lt1 = explore n' (t:visited) (acc++[l]) nlt2
          | S.member (evt2interaction l) lt2 =
            (
              (L.null acc)
              ||
              (checkDependency (acc++[l]) (evt2interaction $ head acc) (evt2interaction l))
            )
            &&
            explore n' (t:visited) (acc++[l]) (S.delete (evt2interaction l) nlt2)
          | otherwise = explore n' (t:visited) (if L.null acc then [] else acc++[l]) nlt2


checkDependency :: [KEvent] -> Interaction -> Interaction -> Bool
checkDependency x lt1 lt2 = (explore x) -- && (checkDependency xs lt1 lt2)
  where explore (n:ns) 
          | (evt2interaction n) == lt1 =
            (sameMachines (evt2interaction n) lt2)
            ||
            (
              let p = (cutPath ns lt2)
              in (L.null p)
                 ||
                 (findDependency evt2interaction dependentInteraction p (evt2interaction n) lt2)
            )
          | otherwise = explore ns
        explore [] = True
-- checkDependency [] _ _ = True

 
cutPath :: [KEvent] -> Interaction -> [KEvent]
cutPath xs lt2 = helper xs []
  where helper (y:ys) acc 
          | (evt2interaction y) == lt2 = acc
          | otherwise = helper ys (acc++[y]) 
        helper [] _ = []
--
-- Find a dependency relation (if any) in quadratic complexity *on a list*
--
findDependency :: (Show a, Show b, Ord a, Ord b) => (a -> b) -> (b -> b -> Bool) -> [a] -> b -> b -> Bool
findDependency tr f list lt1 lt2 = findPath (S.singleton src)
  where
    src = (0,lt1)
    target = ((length list)+1, lt2)
    idxlist (x:xs) i = (i, tr x):(idxlist xs (i+1))
    idxlist [] _ = []
    nodes = let ll = (idxlist list 1) 
            in S.union (S.fromList ll) $ S.insert target (S.singleton src)
    --
    connected (i,x) (j,y) = (i<j) && (f x y)
    --
    findPath border
      | (S.null border) = False
      | otherwise = let newborder = S.fold S.union S.empty $
                                    S.map (\n -> (S.filter (\x -> connected n x)) nodes ) border
                    in (S.member target newborder)
                       ||
                       (findPath newborder)
          

