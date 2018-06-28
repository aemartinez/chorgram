module Dependency where

import CFSM
import TS
import Data.List as L
import Data.Set as S
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
dependentInteraction (m1, m2, _) (m1',m2',_) =
  ((m1 == m1') && (m2 == m2'))
  ||
  (m2 == m1')
  


sameMachines :: Interaction -> Interaction -> Bool
sameMachines (s, r, _) (s', r', _) = (s' == s) && (r == r')


dependencyTS :: Ptp -> Ptp -> TSb -> Configuration -> KEvent -> Set Interaction -> Set Interaction -> Bool
dependencyTS s r oldts n e good bad = 
  let initacc =  if S.member (evt2interaction e) good
                 then [e]
                 else []
      out = explore n [] initacc bad
  in out
  where ts = reduce $ reinit oldts n
        --
        filterEdge visited current (label,node) = not $ L.elem (current,label,node) visited
        --
        --addHead h = L.map (\x -> h:x)
        --
        explore current visited acc nbad = 
          let newpairs =  L.nub $ L.map (\(x,y) -> (current,x,y)) $
                          L.filter (\edge -> filterEdge visited current edge) [(e',n') | (_,e',n') <- S.toList $ deriv current ts]
          in L.and $ L.map (\t -> dispatch t visited acc nbad) newpairs
        --
        dispatch t@(_,l,n') visited acc nbad
          | S.null nbad = True
          | not (L.null acc) && receiver l == s && sender l == r = True
        -- If the receiver (being checked) sends something back the sender, before reaching "bad", it's
        -- fine since there is a unique "selector"
          | S.member (evt2interaction l) good = explore n' (t:visited) (acc++[l]) nbad
          | S.member (evt2interaction l) bad =
            (
              (L.null acc)
              ||
              (checkDependency (acc++[l]) (evt2interaction $ head acc) (evt2interaction l))
            )
            &&
            explore n' (t:visited) (acc++[l]) (S.delete (evt2interaction l) nbad)
          | otherwise = let nacc = if L.null acc
                                   then []
                                   else acc++[l]
                        in explore n' (t:visited) nacc nbad


checkDependency :: [KEvent] -> Interaction -> Interaction -> Bool
checkDependency x good bad = (explore x) -- && (checkDependency xs good bad)
  where explore (n:ns) 
          | (evt2interaction n) == good =
            (sameMachines (evt2interaction n) bad)
            ||
            (
              let p = (cutPath ns bad)
              in (L.null p)
                 ||
                 (findDependency evt2interaction dependentInteraction p (evt2interaction n) bad)
            )
          | otherwise = explore ns
        explore [] = True
-- checkDependency [] _ _ = True

 
cutPath :: [KEvent] -> Interaction -> [KEvent]
cutPath xs bad = helper xs []
  where helper (y:ys) acc 
          | (evt2interaction y) == bad = acc
          | otherwise = helper ys (acc++[y]) 
        helper [] _ = []
--
-- Find a dependency relation (if any) in quadratic complexity *on a list*
--
findDependency :: (Show a, Show b, Ord a, Ord b) => (a -> b) -> (b -> b -> Bool) -> [a] -> b -> b -> Bool
findDependency tr f list good bad = findPath (S.singleton src)
  where
    src = (0,good)
    target = ((length list)+1, bad)
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
          

--- -
-- -- Cut a Tree where at the first occurence of "int", if any, otherwise returns Nothing 
-- --
-- cutTree :: (Eq a, Eq b) => (a -> b) -> Tree a -> b -> Maybe (Tree a)
-- cutTree fun (Node e fs) int 
--   | (fun e) == int = Just (Node e [])
--   | otherwise = case cutForest fun fs int of
--     Just f -> Just (Node e f)
--     Nothing -> Nothing
    
-- cutForest :: (Eq a, Eq b) => (a -> b) -> Forest a -> b -> Maybe (Forest a)
-- cutForest fun fs int = let res = L.filter isJust $ L.map (\tree -> cutTree fun tree int) fs
--                        in if L.null res
--                           then Nothing
--                           else Just (L.map (\(Just x) -> x) res)

