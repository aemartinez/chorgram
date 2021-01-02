--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module implements the well-formedness checking At the moment
-- it is very basic since (almost) the simplest form of
-- well-branchedness is checked and well-sequencedness is not checked.
--
-- TODO: change the codomain of wb to some more informative data type
--       to improve feedback
--

module WellFormedness where

import Misc
import CFSM (Ptp)
import SyntacticGlobalChoreographies
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

data Role = Active | Passive
  deriving (Ord, Eq)

getInteractions :: GC -> Set GC
getInteractions gc =
-- returns the set of interactions occurring in gc
  case gc of
    Emp -> S.empty
    Act (s,r) m -> S.singleton (Act (s,r) m)
    Par gs -> S.unions (S.fromList $ L.map getInteractions gs)
    Bra gs -> S.unions (S.map getInteractions gs)
    Seq gs -> S.unions (S.fromList $ L.map getInteractions gs)
    Rep gc' _ -> getInteractions gc'

isAct :: GC -> Bool
isAct (Act _ _) = True
isAct _ = False

isBra :: GC -> Bool
isBra (Bra _) = True
isBra _ = False

isEmp :: GC -> Bool
isEmp Emp = True
isEmp _ = False

sender :: GC -> Ptp
sender (Act (s, _) _) = s
sender _ = ""

receiver :: GC -> Ptp
receiver (Act (_, r) _) = r
receiver _ = ""

filterPtp :: Ptp -> GC -> GC
filterPtp p gc =
-- projects gc on p
  case gc of
    Emp -> Emp
    Act (s,r) _ ->
      if p == s || p == r
      then gc
      else Emp
    Par gs -> Par (L.map (filterPtp p) gs)
    Bra gs -> Bra (S.map (filterPtp p) gs)
    Seq gs -> Seq (L.map (filterPtp p) gs)
    Rep gc' q -> Rep (filterPtp p gc') q

firstOnly :: GC -> GC
firstOnly gc =
-- returns a g-choreography with the same "top-level"
-- structure of gc with the "first" interations
-- eg if gc = A -> B: m | (Emp ; C -> D: n) then
-- firstOnly gc = A -> B: m | C -> D: n 
  case gc of
    Emp -> gc
    Act (_, _) _ -> gc
    Par gs -> Par (L.map firstOnly gs)
    Bra gs -> Bra (S.map firstOnly gs)
    Seq gs ->
      case gs of
        [] -> Emp
        [g] ->
          let h = firstOnly $ simplifyGC g
          in
            case h of
              Emp -> Emp
              _ -> h
        g:gs' -> 
          let h = firstOnly $ simplifyGC g
          in
            case h of
              Emp -> firstOnly (Seq gs')
              _ -> h
    Rep gc' q -> Rep (firstOnly $ simplifyGC gc') q

simplifyGC :: GC -> GC
simplifyGC gc =
--
-- simplifies gc by flattening nested | and + according to the
-- following structural congruence rules are :
-- 
-- 	(o) + (o) = (o)
-- 	( GC, _|_, (o) ) abelian monoid
-- 	( GC, _;_, (o) ) monoid
--
  case gc of
    Emp -> Emp
    Act (_, _) _ -> gc
    Seq gcs ->
      let gcs' = [g | g <- (L.map simplifyGC gcs), g /= Emp]
      in
        case gcs' of
          [] -> Emp
          [gc'] -> gc'
          _ -> Seq gcs'
    Par gcs ->
      let gcs' = [g | g <- (L.map simplifyGC gcs), g /= Emp]
      in
        case gcs' of
          [] -> Emp
          [gc'] -> gc'
          _ -> Par gcs'
    Bra gcs ->
      let gcs' = S.map simplifyGC gcs
          rmBra g =
            case g of
              Bra gs' -> gs'
              _ -> S.singleton g
          (bra, oth) = S.partition isBra gcs'
          flatBra = S.unions $ S.map rmBra bra
      in Bra (S.union flatBra oth)
    Rep gc' p ->
      let body = simplifyGC gc'
      in
        case body of
          Emp -> Emp
          _ -> Rep body p

check :: Role -> Ptp -> Set GC -> Bool
check c p branching =
-- PRE:  S.size branching > 1    AND   all g-choreography in 'branching' are simplified
-- POST: checks if p is active or passive in 'branching'
-- TODO: return lists of problematic branches when the condition is violated
  let pBra = S.map (simplifyGC . firstOnly . (filterPtp p)) branching
      emptyness =
        case c of
          Active -> L.all (not . isEmp) pBra
          Passive -> (L.all isEmp pBra) || (L.all (not . isEmp) pBra)
      ints = S.toList $ S.map getInteractions pBra
      f =
        case c of
          Active -> sender
          Passive -> receiver
      io = L.all (L.all (\a -> isAct a && p == f a)) ints
      pFSTs = M.fromList $ L.zip (range $ S.size pBra) ints
      nonAmbiguous =
        case pairwiseDisjoint pFSTs of
          Nothing -> True
          _ -> False
  in emptyness && io && nonAmbiguous

isActive :: Ptp -> Set GC -> Bool
isActive p branching = check Active p branching
  -- case check Active p branching of
  --   [] -> True
  --   _ -> False

isPassive :: Ptp -> Set GC -> Bool
isPassive p branching = check Passive p branching
  -- case check Passive p branching of
  --   [] -> True
  --   _ -> False

wb :: GC -> Maybe (GC, GC)
wb gc = Nothing

wf :: GC -> Maybe (GC, GC)
wf gc =
-- checks for well-forkedness and returns the set
-- of interactions occurring on more than one thread
  case gc of
    Emp -> Nothing
    Act (_,_) _ -> Nothing
    Par gs ->
      let f = M.fromList $ (L.zip (range $ L.length gs) (L.map getInteractions gs))
      in
        case pairwiseDisjoint f of
          Nothing -> Nothing
          Just (i,j) -> Just (gs!!i, gs!!j)
    Bra gs ->
      case S.toList gs of
        [] -> Nothing
        g:_ ->
          if wf g == Nothing
          then wf (Bra (S.delete g gs))
          else wf g
    Seq gs ->
      case gs of
        [] -> Nothing
        g:l ->
          if wf g == Nothing
          then wf (Seq l)
          else wf g
    Rep gc' _ -> wf gc'


wb :: GC -> Maybe GC
wb gc =
-- checks for well-branchedness and returns the set
-- of interactions occurring on more than one thread
  case gc of
    Emp -> Nothing
    Act (_,_) _ -> Nothing
    Par gs ->
      case L.find (\x -> x /= Nothing) (L.map wb gs) of
        Nothing -> Nothing
        Just x -> x
    Bra gs ->
      let ptps = gcptp S.empty gc
          gc' = S.map firstOnly gs
          getActive = S.filter (\p -> isActive p (S.map simplifyGC gc')) ptps
          getPassive = S.filter (\p -> isPassive p (S.map (\g -> simplifyGC $ filterPtp p g) gs)) ptps
      in
        if S.size getActive /= 1
        then Just Emp
        else
          if (S.size getPassive) /= (S.size ptps) - 1
          then Just Emp
          else Nothing
    Seq gs ->
      case L.find (\x -> x /= Nothing) (L.map wb gs) of
        Nothing -> Nothing
        Just x -> x
    Rep gc' _ -> wb gc'


