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
import CFSM (Ptp, isSend, senderOf, receiverOf, isSend, isReceive)
import SyntacticGlobalChoreographies
import PomsetSemantics
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

data Role = Active | Passive
  deriving (Ord, Eq)

interactionsOf :: GC -> Set GC
interactionsOf gc =
-- returns the set of interactions occurring in gc
  case gc of
    Emp -> S.empty
    Act (s,r) m -> S.singleton (Act (s,r) m)
    Par gs -> S.unions (S.fromList $ L.map interactionsOf gs)
    Bra gs -> S.unions $ M.elems $ (M.map interactionsOf gs)
    Seq gs -> S.unions (S.fromList $ L.map interactionsOf gs)
    Rep gc' _ -> interactionsOf gc'

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
    Bra gs -> Bra (M.map (filterPtp p) gs)
    Seq gs -> Seq (L.map (filterPtp p) gs)
    Rep gc' q -> Rep (filterPtp p gc') q

firstOnly :: GC -> GC
firstOnly gc =
-- returns a g-choreography with the same "top-level"
-- structure of gc with the "first" interations
-- eg if gc = A -> B: m | ((o) ; C -> D: n) then
-- firstOnly gc = A -> B: m | C -> D: n 
  case gc of
    Emp -> gc
    Act (_, _) _ -> gc
    Par gc -> Par (L.map firstOnly gc)
    Bra gc -> Bra (M.map firstOnly gc)
    Seq gc ->
      let
        gc' = L.filter (\x -> not. isEmp $ simplifyGC x) gc
      in
        case gc' of
          [] -> Emp
          _ -> firstOnly $ head gc'
    Rep gc' q -> Rep (firstOnly $ simplifyGC gc') q

lastOnly :: GC -> GC
lastOnly gc =
-- returns a g-choreography with the same "bottom-level"
-- structure of gc with the "last" interations
-- eg if gc = (o) | (G ; C -> D: n ; (o)) then
-- lastOnly gc = (o) | C -> D: n 
  case gc of
    Emp -> gc
    Act (_, _) _ -> gc
    Par gc -> Par (L.map lastOnly gc)
    Bra gc -> Bra (M.map lastOnly gc)
    Seq gc ->
      let
        gc' = L.filter (\x -> not. isEmp $ simplifyGC x) gc
      in
        case gc' of
          [] -> Emp
          _ -> lastOnly $ L.last gc'
    Rep gc' q -> Rep (lastOnly gc') q

simplifyGC :: GC -> GC
simplifyGC gc =
--
-- simplifies gc by flattening nested | and + according to the
-- following structural congruence rules are :
-- 
-- 	(o) + gc = gc
-- 	( GC, _|_, (o) ) abelian monoid
-- 	( GC, _;_, (o) ) monoid
--
  case gc of
    Emp -> Emp
    Act (_, _) _ -> gc
    Seq gcs ->
      let
        gcs' = [g | g <- (L.map simplifyGC gcs), g /= Emp]
      in
        case gcs' of
          [] -> Emp
          [gc'] -> gc'
          _ -> Seq gcs'
    Par gcs ->
      let
        gcs' = [g | g <- (L.map simplifyGC gcs), g /= Emp]
      in
        case gcs' of
          [] -> Emp
          [gc'] -> gc'
          _ -> Par gcs'
    Bra gcs ->
      let
        shift k m =
          M.fromList [(i+k,v) | (i,v) <- M.toList m]
        (bra, oth) =
          L.partition isBra (M.elems $ M.map simplifyGC gcs)
        ms =
          [m | Bra m <- bra]
        aux =
          M.fromList $ L.zip ([1 .. (L.length oth)]) oth
        gcs' =
          L.foldl (\x y -> M.union x (shift (M.size x) y)) aux ms
      in
        if (S.size $ S.fromList $ M.elems gcs') == 1 
        then L.head $ M.elems gcs'
        else Bra gcs'
    Rep gc' p ->
      let body = simplifyGC gc'
      in
        case body of
          Emp -> Emp
          _ -> Rep body p

naiveWB :: Role -> Ptp -> Map Label GC -> Bool
-- PRE:  M.size branching > 1    AND   all g-choreography in 'branching' are simplified
-- POST: checks if p is active or passive in 'branching'
-- TODO: return lists of problematic branches when the condition is violated
naiveWB Active p branching =
  let
    pBra = M.map (simplifyGC . firstOnly . (filterPtp p)) branching
    gcs = M.elems pBra
    emptyness = L.all (not . isEmp) gcs
    ints = M.map interactionsOf pBra
    outputOnly = L.all (\a -> L.all (\x -> isAct x && p == (sender x)) a) ints
    disjointness =
      case pairwiseDisjoint ints of
        Nothing -> True
        _ -> False
  in emptyness && outputOnly && disjointness

naiveWB Passive p branching =
  let
    pBra = M.map (simplifyGC . firstOnly . (filterPtp p)) branching
    gcs = M.elems pBra
    emptyness = (L.all isEmp gcs) || (L.all (not . isEmp) gcs)
    ints = M.map interactionsOf pBra
    inputOnly = L.all (\a -> S.foldl (\b x -> b && isAct x && (p == (receiver x))) True a) ints
    disjointness =
      case pairwiseDisjoint ints of
        Nothing -> True
        _ -> False
  in emptyness && inputOnly && disjointness

-- chkBranching :: Role -> Ptp -> [GC] -> Maybe String
-- chkBranching r p branching =
--   aux Nothing (L.map (firstOnly . filterPtp p) branching)
--   where
--     aux r ls =
--       case r of
--         Just _ -> r
--         _ ->
--           case ls of
--             [] -> Nothing
--             [_] -> Nothing
--             x:y:ls' ->
--               if x == y
--               then Just ("Ambiguity: " ++ (show x) ++ "appears as first action of " ++ p ++ " on more than one branch")
--               else aux (aux r (x:ls')) (y:ls')


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
      let tmp = M.map cutFirst gs
      in (S.unions (S.map fst (S.fromList $ M.elems tmp)), Bra (M.map snd tmp))
    Seq gs ->
      let (f,r) = cutFirst (L.head gs)
      in (f, Seq (r:(L.tail gs)))
    Rep gc' p ->
      let (f,r) = cutFirst gc'
      in (f, Rep r p)

dependency :: (Set Ptp, Set Ptp) -> GC -> (Set Ptp, Set Ptp)
dependency (min_int, oth) gc =
-- computes the set of senders of minimal interactions and those
-- participants whose actions causally depend on actions of the ones
-- in min_int
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
      let tmp = M.map (dependency (min_int, oth)) gs
      in (S.unions $ S.map fst (S.fromList $ M.elems tmp),
          S.unions $ S.map snd (S.fromList $ M.elems tmp))
    Seq gs -> L.foldl (\(m,o) g -> dependency (m,o) g) (min_int, oth) gs
    Rep gc' p ->
      if S.member p oth
      then dependency (min_int, oth) gc'
      else dependency (S.insert p min_int, oth) gc'

-- ws :: Set GC -> Set GC -> GC -> Set GC
-- ws acc tbc gc =
-- -- checks for well-sequencedness and returns a g-choreography that
-- -- cannot be put in sequence with its subsequent g-choreography (if any)
--   let
--     (f, gc') =
--       cutFirst $ simplifyGC gc
--     rcv =
--       S.map receiver acc
--     tbc' =
--       S.union tbc (S.filter (\a -> not $ S.member (sender a) rcv) f)
--     acc' =
--       S.union acc (f S.\\ tbc')
--   in
--     if simplifyGC gc' == Emp
--     then S.union acc' tbc'
--     else ws acc' tbc' gc'

ws :: GC -> Bool
ws gc =
  -- checks for well-sequencedness and returns a g-choreography that
  -- cannot be put in sequence with its subsequent g-choreography (if
  -- any).
  case gc of
    Emp -> True
    Act _ _ -> True
    Par gcs -> L.all ws gcs
    Bra gcs -> L.all ws gcs
    Seq gcs ->
      case gcs of
        [] -> True
        [gc'] -> ws gc'
        gc':gc'':gcs' ->
          (ws gc') && (wsAux gc' gc'') && (ws (Seq (tail gcs)))
    Rep gc' _ -> ws gc'

wsAux :: GC -> GC -> Bool
wsAux gc gc' = -- error $ show $ L.all outCond ps'
  -- NOTE: the implementation differs from the formal definition
  -- because the check that minimal imputs of gc' depend on some
  -- output of gc is done syntactically instead of using pomsets.
  (isEmp $ simplifyGC gc) ||
  (isEmp $ simplifyGC gc') ||
  (inCond && L.all outCond ps')
  where
    (sem, e') = pomsetsOf gc 1 0
    (sem', _) = pomsetsOf gc' 1 (e'+1)
    (ps, ps') = (S.toList $ sem, S.toList $ sem')
    ptpgc = ptpOf gc
    outCond r' =
      let
        l' = (labelOf r')
        outs' = S.filter (\e -> isSend (l'!e) && S.member (senderOf (l'!e)) ptpgc) (eventsOf r')
        chk r =
          let
            l = (labelOf r)
            outs = S.filter (\e -> isSend (l!e)) (eventsOf r)
          in
            S.null (
              (S.fromList [(e,e') | e <- S.toList outs, e' <- S.toList outs'])
              S.\\
              (orderOf $ seqLeq (r, r'))
            )
      in
        L.all chk sem
    inCond =
      let
        ptps = ptpOf gc
        min = interactionsOf $ firstOnly gc'
        aux (Act (s,r) _ ) =
          (S.member r ptps) || (S.member s ptps)
      in
        L.all aux (S.toList min)
        
wf :: GC -> Maybe (GC, GC)
wf gc =
-- checks for well-forkedness and returns the set
-- of interactions occurring on more than one thread
  case gc of
    Emp -> Nothing
    Act (_,_) _ -> Nothing
    Par gs ->
      let f = M.fromList $ (L.zip (range $ L.length gs) (L.map interactionsOf gs))
      in
        case pairwiseDisjoint f of
          Nothing -> Nothing
          Just (i,j) -> Just (gs!!i, gs!!j)
    Bra gcs ->
      case M.elems gcs of
        [] -> Nothing
        g:gs' ->
          if wf g == Nothing
          then wf (Bra (M.fromList $ L.zip ([1 .. L.length gs']) gs'))
          else wf g
    Seq gs ->
      case gs of
        [] -> Nothing
        g:l ->
          if wf g == Nothing
          then wf (Seq l)
          else wf g
    Rep gc' _ -> wf gc'



wb :: GC -> Maybe String
wb gc =
-- checks for well-branchedness and returns the set
-- of interactions occurring on more than one thread
  case gc of
    Emp -> Nothing
    Act (_,_) _ -> Nothing
    Par gs ->
      case (L.find (\x -> x /= Nothing) (L.map wb gs)) of
        Nothing -> Nothing
        _ -> Just (L.foldr (\l l' -> l ++ "\n" ++ l') "" [x | (Just x) <- (L.filter (\x -> x /= Nothing) (L.map wb gs))])
    Bra gcs ->
      let ptps = ptpOf gc
          gcs' = M.map simplifyGC gcs
          getActive = S.filter (\p -> naiveWB Active p gcs') ptps
          getPassive = S.filter (\p -> naiveWB Passive p gcs') ptps
      in
        let mkList = \x y -> x ++ " " ++ y
        in
          case S.size getActive of
            0 ->
              if L.all (== (L.head $ M.elems gcs)) (L.tail $ M.elems gcs)
              then Nothing
              else Just "No active participant"                      
            1 ->
              if (S.size getPassive) /= (S.size ptps) - 1
              then Just ("Either some non-active participant is not passive," ++
                         "\n\tor no active or passive participants:" ++
                         "\n\t\tactive: " ++ (S.foldr mkList "" getActive) ++
                         "\n\t\tpassive: " ++ (S.foldr mkList "" getPassive)
                        )
              else
                if L.all (== (L.head $ M.elems gcs)) (L.tail $ M.elems gcs)
                then Nothing
                else
                  let
                    pgc p = M.elems $ M.map (simplifyGC . filterPtp p ) gcs'
                    aux p = L.all (== (L.head $ (pgc p))) (L.tail $ (pgc p))                            
                    uniform = S.filter aux getActive
                  in
                    case S.toList (getActive S.\\ uniform) of
                      [] -> Nothing
                      [_] -> Nothing
                      _ -> Just ("There are several active participants: " ++ (S.foldr mkList "" (uniform S.\\ getActive)))
            _ -> Just ("There are several active participants: " ++ (S.foldr mkList "" getActive))
    Seq gs ->
      case (L.find (\x -> x /= Nothing) (L.map wb gs)) of
        Nothing -> Nothing
        _ -> Just (L.foldr (\l l' -> l ++ "\n" ++ l') "" [x | (Just x) <- (L.filter (\x -> x /= Nothing) (L.map wb gs))])
    Rep gc' _ -> wb gc'

