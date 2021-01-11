--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module implements the well-formedness checking At the moment
-- it is very basic since (almost) the simplest form of
-- well-branchedness is checked and well-sequencedness is not checked.
--
-- At the moment there are some limitations, but in some sense this
-- implementation complements PomCho's closure conditions because the
-- checking is just syntactic. Here are the limitations we have so
-- far (TODO):
--
--  - well-sequencedness not implemented (actually it is just the
--    constant function \x -> True)
--  - the function wb to check for well-branchedness just returns a
--    boolean; it would be good that in case the properties do not
--    hold it returned more informative feedback to highlight the
--    problem
--  - try to generalise the checking of well-branchedness as per the
--    paper of JLAMP 2019; this is non trivial
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

-- Basic functions to check the nature of a g-choreography
isAct :: GC -> Bool
isAct (Act _ _) = True
isAct _ = False

isBra :: GC -> Bool
isBra (Bra _) = True
isBra _ = False

isEmp :: GC -> Bool
isEmp Emp = True
isEmp _ = False

-- Extracting information from an action
sender :: GC -> Ptp
sender (Act (s, _) _) = s
sender gc = error $ "\'sender\' applied to " ++ (show gc)

receiver :: GC -> Ptp
receiver (Act (_, r) _) = r
receiver gc = error $ "\'receiver\' applied to " ++ (show gc)

filterPtp :: Ptp -> GC -> GC
filterPtp p gc =
-- projects gc on p...without splitting interactions
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
      in
        if all isEmp gcs'
        then Emp
        else Bra (S.union flatBra oth)
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

isPassive :: Ptp -> Set GC -> Bool
isPassive p branching = check Passive p branching

cutFirst :: GC -> (Set GC, GC)
cutFirst gc =
-- separates the first interactions of gc from the rest
  case simplifyGC gc of
    Emp -> (S.empty, Emp)
    Act (_, _) _ -> (S.singleton $ simplifyGC gc, Emp)
    Par gs ->
      let tmp = L.map cutFirst gs
      in (S.unions (L.map fst tmp), Par (L.map snd tmp))
    Bra gs ->
      let tmp = S.map cutFirst gs
      in (S.unions (S.map fst tmp), Bra (S.map snd tmp))
    Seq gs ->
      let (f,r) = cutFirst (L.head gs)
      in (f, Seq (r:(L.tail gs)))
    Rep gc' p ->
      let (f,r) = cutFirst gc'
      in (f, Rep r p)

dependency :: (Set Ptp, Set Ptp) -> GC -> (Set Ptp, Set Ptp)
dependency (min_int, oth) gc =
-- computes the set of senders of minimal interactions s and those
-- participants whose actions causally depend on actions of the ones
-- in min
  case gc of
    Emp -> (min_int, oth)
    Act (s, r) _ ->
      if S.member s oth
      then (min_int, S.insert r oth)
      else (S.insert s min_int, S.insert r oth)
    Par gs ->
      let tmp = L.map (dependency (min_int, oth)) gs
      in (S.unions $ L.map fst tmp, S.unions $ L.map snd tmp)
    Bra gs -> 
      let tmp = S.map (dependency (min_int, oth)) gs
      in (S.unions $ S.map fst tmp, S.unions $ S.map snd tmp)
    Seq gs -> L.foldl (\(m,o) g -> dependency (m,o) g) (min_int, oth) gs
    Rep gc' p ->
      if S.member p oth
      then dependency (min_int, oth) gc'
      else dependency (S.insert p min_int, oth) gc'

-- ws :: Set GC -> Set GC -> GC -> Set GC
-- ws acc tbc gc =
-- -- checks for well-sequencedness and returns a g-choreography that
-- -- cannot be put in sequence with its subsequent g-choreography (if any)
--   let (f, gc') = cutFirst gc
--       tbc' = S.union tbc (S.filter (\a -> not $ S.member (sender a) (S.map receiver acc)) f)
--       acc' = S.union acc (f S.\\ tbc')
--   in
--     if simplifyGC gc' == Emp
--     then S.union acc' tbc'
--     else ws acc' tbc' gc'

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
          gc' = S.map (simplifyGC . firstOnly) gs
          getActive = S.filter (\p -> isActive p gc') ptps
          getPassive = S.filter (\p -> isPassive p gs) ptps
      in
        if S.size getActive /= 1
        then Just Emp
        else
          if (S.size getPassive) /= (S.size ptps) - 1
          then Just Emp
          else
            if L.all (\g -> (fst $ dependency (S.empty, S.empty) g) == getActive) gs
            then Nothing
            else Just Emp
    Seq gs ->
      case L.find (\x -> x /= Nothing) (L.map wb gs) of
        Nothing -> Nothing
        Just x -> x
    Rep gc' _ -> wb gc'


