
--
-- Author: Emilio Tuosto <emilio@le.ac.uk>
--
-- This module would like to implement the semantics defined in
-- ICE16...but it does not do it yet
--

module SemanticGlobalGraphs where

import Data.Set as S
import Data.List as L
import Data.Maybe
import SyntacticGlobalGraphs
import Misc
import CFSM
import DotStuff
import Data.Map.Strict as M

-- TODO: 

--
-- Types for control points and events with the assumption that
-- (i,(Tau,(i,i),m)) is a "control point" event for branch
-- if m == "+" and for fork if m == "|"
--
type K = Int
type E = (K, Maybe Action)

--
-- Hypergraphs are sets of hyperedges on events, but we keep
-- computing also its min, max, fst, and lst sets: ie an hg
-- has the form (R,min,max,fst,lst)
--
type HE = (Set E, Set E)
type HG = (Set HE, Set E, Set E, Set HE, Set HE)

--
-- A type for bijections on control points; we simply assume
-- that a mu is a control point (whose the corresponding merge/join
-- is -mu)
--
type Mu = K

--
-- sameAct aV returns the subject of the actions of aV provided that those are
-- the same actions
--
sameAct :: Set E -> Maybe Action
sameAct aV =
  if (S.size acts) == 1 then S.elemAt 0 acts else Nothing
  where acts = (S.map (\e -> if (isJust $ snd e) then (snd e) else Nothing) aV) S.\\ (S.singleton Nothing)

-- TODO: to be finished
sameThread :: HG -> E -> E -> Bool
sameThread hg e e' =
  (S.member (e,e') hbrel) || (S.member (e',e) hbrel) || (False)
  where hbrel = hb hg

-- to be moved
--
-- reflectsHG aV aV' checks that two lists of events are partitions and form a bijection between aV and aV'
--
reflectHG :: HG -> [Set E] -> [Set E] -> Maybe ([Set E] -> [Set E])
reflectHG hg aV aV' =
  if (L.length aV) == (L.length aV') && (checkPart aV) && (checkPart aV') && (eachSameAct pairs) && (upwardClose pairs)
  then f
  else Nothing
  where
    f = Nothing
    pairs = L.zip aV aV'
    eachSameAct l =
      case l of
        []         -> True
        (aX,aY):l' -> if (sameAct aX == sameAct aY) then (eachSameAct l') else False
    checkPart l =
      case l of
        []    -> True
        aX:l' -> (S.null (S.intersection aX (S.unions l'))) && checkPart l'
    upwardClose l =
      case l of
        []         -> True
        (aX,aY):l' -> False -------------------------------

ptpsOf :: E -> Maybe (Ptp, Ptp)
ptpsOf (_, Just ((s,r), _, _)) = Just (s,r)
ptpsOf (_, Nothing)            = Nothing

sbjOf :: E -> Maybe Ptp
sbjOf (_, Nothing)                  = Nothing
sbjOf (_, Just (_, Tau, _))         = Nothing
sbjOf (_, Just ((s,_), Send, _))    = Just s
sbjOf (_, Just ((_,r), Receive, _)) = Just r
sbjOf (_, Just ((s,_), LoopSnd, _)) = Just s
sbjOf (_, Just ((_,r), LoopRcv, _)) = Just r

emptyHG :: (Set HE, Set E, Set E, Set HE, Set HE)
emptyHG = (S.empty, S.empty, S.empty, S.empty, S.empty)

relOf :: HG -> Set HE
relOf (rels,_,_,_,_) = rels

minOf :: HG -> Set E
minOf (_,mins,_,_,_) = mins

maxOf :: HG -> Set E
maxOf (_,_,maxs,_,_) = maxs

fstOf :: HG -> Set HE
fstOf (_,_,_,fsts,_) = fsts

lstOf :: HG -> Set HE
lstOf (_,_,_,_,lsts) = lsts

isOutEvent :: E -> Bool
isOutEvent (_,Nothing) = False
isOutEvent (_, Just ((_,_), d, _)) = d == Send || d == LoopSnd

isInpEvent :: E -> Bool
isInpEvent (_,Nothing) = False
isInpEvent (_, Just ((_,_), d, _)) = d == Receive || d == LoopRcv

eventsOf :: Set HE -> [E]
eventsOf rel = S.foldr (\l l' -> l++l') [] set
  where set = S.map (\(es,es')->((S.toList es) ++ (S.toList es'))) rel
  
-- Some auxiliary functions implementing those defined in the ICE16 paper

cp :: E -> K
cp = fst

actOf :: E -> Maybe Action
actOf = snd

csFst :: HE -> Set E
csFst = fst

csSnd :: HE -> Set E
csSnd = snd

(@@) :: HG -> Ptp -> Set HE
hg @@ p = S.map (\(v1,v2) -> (S.map aux v1, S.map aux v2)) (relOf hg)
  where aux e = let q = sbjOf e
                in if (((isJust q) && fromJust q == p) || q == Nothing)
                   then e
                   else (if isOutEvent e
                         then (cp e, Nothing)
                         else (if isInpEvent e then ((-(cp e)), Nothing) else error "!!!")
                        )
                                                                                                           
communicationsOf :: HG -> Ptp -> [E]
communicationsOf hg p = L.filter (\e -> isOutEvent e || isInpEvent e) (eventsOf (hg @@ p))

-- # is not used
(#) :: HG -> HG -> HG
hg # hg' = ( rel, minOf hg, maxOf hg', fstOf hg, lstOf hg' )
  where rel =  S.fromList [(fst es, snd es')| es <- (S.toList $ relOf hg), es' <- (S.toList $ relOf hg'), (Misc.intersect (snd es) (fst es')) ]

seqHG :: HG -> HG -> HG
seqHG hg hg' = ( rel, minOf hg, maxOf hg', fstOf hg, lstOf hg' )
  where rel = S.unions [relOf hg, relOf hg', S.fromList l]
        l   = [( S.singleton e, S.singleton e' ) |
               e  <- eventsOf $ lstOf hg,  isJust $ actOf e,
               e' <- eventsOf $ fstOf hg', isJust $ actOf e',
               (sbjOf e == sbjOf e')]

-- reflexive and transitive closure of the order induce by a hg
precHG :: HG -> (E, E) -> Bool
precHG hg ( e, e' ) = (e==e') || (reach (S.singleton e) e' (hb hg))

-- reach es e' ker returns true if e is reachable from one of the events in es
reach :: Set E -> E -> Set (E, E) -> Bool
reach es e' ker = not(S.null es) && ( e==e' || reach (S.union es' (stepHG e ker)) e' ker )
  where ( e, es' ) = (head esl, S.fromList $ tail esl)
        esl        = S.toList es
                                
stepHG :: E -> Set (E, E) -> Set E
stepHG e ker = S.map snd (S.filter (\(e',_) -> e == e') ker)

-- happens-before relation; \widehat relation in the paper
hb :: HG -> Set (E,E)
hb hg = S.fromList [ (e,e') | (es,es') <- S.toList $ relOf hg, e <- S.toList es, e' <- S.toList es' ]

-- ws checks that two HG can be composed sequentially
ws :: HG -> HG -> Bool
ws pg pg' = L.all (precHG (seqHG pg pg')) chkl
  where chkl = [(e,e') | e  <- L.filter isOutEvent (eventsOf (relOf pg)),
                         e' <- L.filter isInpEvent (eventsOf (relOf pg'))]
  -- where chkl = [( e, e' ) | e  <- L.concat $ S.toList (S.map (S.toList . csFst) (lstOf $ pg)),
  --                           e' <- L.concat $ S.toList (S.map (S.toList . csSnd) (fstOf $ pg'))]

unionHG :: Maybe HG -> Maybe HG -> Maybe HG
unionHG hg hg' 
  | (isJust hg) && (isJust hg') =
      let (hg1,hg1') = (fromJust hg, fromJust hg')
      in Just (
               S.union (relOf hg1) (relOf hg1'),
               S.union (minOf hg1) (minOf hg1'),
               S.union (maxOf hg1) (maxOf hg1'),
               S.union (fstOf hg1) (fstOf hg1'),
               S.union (lstOf hg1) (lstOf hg1')
              )
  | otherwise                   = Nothing

unionsHG :: [Maybe HG] -> Maybe HG
unionsHG [] = Just emptyHG
unionsHG (hg:hgs) = if (isJust hg) && (isJust hg') then unionHG hg hg' else Nothing
  where hg' = unionsHG hgs

semList :: Bool -> Mu -> [GG] -> P -> (Mu,[Maybe HG])
semList sloppy mu ggs ptps = case ggs of
                       []        -> (mu,[])
                       (gg:ggs') -> (mu'', ([Just hg'] ++ rest))
                           where ( mu', hg' )   = sem sloppy mu gg ptps
                                 ( mu'', rest ) = semList sloppy mu' ggs' ptps

--
-- The semantic function [[_]] of ICE16; it is assumed that some
-- syntactic checks are performed. Eg in Rep g' p we assume p is
-- one of the participants in g'.
-- The labels of the evens corresponds to those used in proj (again
-- this is the case for iteration)
--
sem :: Bool -> Mu -> GG -> P -> (Mu, HG)
sem sloppy mu gg ptps =
  case gg of    -- Note: no longer normalisation and factorisation...factorise $ normGG gg
   Emp         -> ( mu, emptyHG )
   Act (s,r) m -> ( i, ( rel, S.singleton e, S.singleton e', rel, rel ) )
       where i   = 1 + mu
             e   = ( i, Just ( ( s, r ), Send, m ) )
             e'  = ( i, Just ( ( s, r ), Receive, m ) )
             rel = S.singleton ( S.singleton e, S.singleton e' )
   Par ggs     -> (i, hgu)
     where ( mu', l ) = semList sloppy mu ggs ptps
           i          = 1 + mu'
           hgu_       = unionsHG l
           (e1,e2)    = ((S.singleton (i,Nothing), minOf $ fromJust hgu_), (maxOf $ fromJust hgu_, S.singleton ((-i), Nothing)))
           hgu        = if isJust hgu_
                        then (S.union (relOf $ fromJust hgu_) (S.fromList [e1, e2]),
                              minOf $ fromJust hgu_,
                              maxOf $ fromJust hgu_,
                              S.singleton e1,
                              S.singleton e2
                             )
                        else error (msgFormat SGG "Something wrong in a fork: " ++ (show (Par ggs)))
   Bra ggs     -> (i, fromJust hg')
     where ( mu', l )  = semList sloppy mu (S.toList ggs) ptps
           i   = 1 + mu'
           hgu = unionsHG l
           hg' = if sloppy || (wb ggs && isJust hgu)
                 then unionHG hgu
                      (Just ( S.fromList $ L.concat $ L.map aux l, e, e', fstOf $ fromJust hgu, lstOf $ fromJust hgu ))
                 else error (msgFormat SGG "Violation of well-branchedness: " ++ show (Bra ggs))
           e   = S.singleton (i, Nothing)
           e'  = S.singleton ((-i), Nothing)
           aux = \x -> if isJust x then let x' = fromJust x in [(e, minOf x')] ++ [(maxOf x', e')] else error (msgFormat SGG "ERROR ...")
   Seq ggs     -> case ggs of
                   []            -> ( mu, emptyHG )
                   [g']          -> sem sloppy mu g' ptps
                   gg':gg'':ggs' -> if sloppy || (ws pg pg')
                                    then hgs
                                    else error (msgFormat SGG "Violation of well-sequencedness: " ++ show (Seq ggs))
                     where hgs           = (mu'', (seqHG pg pg'))
                           ( mu', pg )   = sem sloppy mu gg' ptps
                           ( mu'', pg' ) = sem sloppy mu' (Seq (gg'':ggs')) ptps
   Rep gg' p -> ( mu', hgr )
     where ps                = ggptp S.empty gg'
           ( mu', hgb )     = if sloppy || S.member p ps
                              then sem sloppy mu gg' ptps
                              else error (msgFormat SGG "Participant " ++ p ++ " is not in the loop: " ++ show (Rep gg' p))
           ( i, suf )       = ( 1+mu' , show i )
           ( eL, eE )       = (S.singleton (i, Just ( ( p , p ) , LoopSnd , lpref ++ suf )), S.singleton ((-i), Just ( ( p , p ) , LoopRcv , epref ++ suf )))
           rel              = S.fromList ([( S.singleton e , eE ) | e <- S.toList $ maxOf $ hgb] ++
                                          [( eL , S.singleton e ) | e <- S.toList $ minOf $ hgb] ++
                                          [( eE , eL )]
                                         )
           hgr              = (S.union rel (relOf hgb), eL, eE, (fstOf hgb), (lstOf hgb))

-- DOT format

he2dot :: HE -> Map String String -> Int -> String
he2dot (evs,evs') flines i =
  "\t" ++ con ++ heV ++
  (L.concat $ L.map (\n -> "\t" ++ n ++ " -> " ++ con ++ " [arrowhead=none, color=" ++ flines!hecol ++ "]\n") src) ++
  (L.concat $ L.map (\n -> "\t" ++ con ++ " -> " ++ n ++ " [color=" ++ flines!hecol ++ "]\n") dst)
  where src = S.toList $ S.map cp2dot evs
        dst = S.toList $ S.map cp2dot evs'
        con = "hgconn" ++ replaceChar '-' '_' (show i)

hg2dot :: HG -> Map String String -> String
hg2dot (rel,_,_,_,_) flines =
  header ++ stuff ++ footer
  where header   = "digraph sem {\nnode [width=" ++ nodeSize ++ ", height=" ++ nodeSize ++ "]\n\n"
        footer   = "\n}\n\n/*\n" ++ (show rel) ++ "\n*/"
        nodes    = S.map (\ev -> "\t" ++ (ev2dot ev flines)) (S.foldr S.union S.empty (S.map (\(x,y) -> (S.union x y)) rel))
        edges    = S.fromList [he2dot (S.elemAt i rel) flines i | i <- [0 .. (S.size rel) -1] ]
        stuff    = L.concat $ (S.toList nodes) ++ (S.toList edges)
        nodeSize = flines!ggsizenode


cp2dot :: E -> String
cp2dot ev = "node" ++ (replaceChar '-' '_' $ show $ fst ev) ++ suf
  where suf = if isNothing $ snd ev
              then ""
              else let act = (fromJust $ snd ev) in
                    case act of
                     ( _, Send, _ )    -> "snd"
                     ( _, Receive, _ ) -> "rcv"
                     ( _, LoopSnd, _ ) -> subjectOf act
                     ( _, LoopRcv, _ ) -> subjectOf act
                     _                 -> error (msgFormat SGG "ERROR " ++ show ev)

ev2dot :: E -> Map String String -> String
ev2dot ev@(_, Nothing) _  = cp2dot ev ++ cpV
ev2dot ev@(_, Just ((s,r), d, m)) flines =
  case d of
   LoopSnd -> cp2dot ev ++ " [label=\"" ++ (rmChar '\"' $ show s) ++ actstr  ++ "]\n"
   LoopRcv -> cp2dot ev ++ " [label=\"" ++ (rmChar '\"' $ show s) ++ actstr  ++ "]\n"
   _       -> cp2dot ev ++ " [label=\"" ++ (rmChar '\"' $ show s) ++ (flines!ptpsep) ++ (rmChar '\"' $ show r) ++ actstr  ++ "]\n"
  where actstr = case d of
                  Send    -> " ! " ++ (rmChar '\"' $ show m) ++ "\", shape=" ++ (flines!evshape)
                  Receive -> " ? " ++ (rmChar '\"' $ show m) ++ "\", shape=" ++ (flines!evshape)
                  LoopSnd -> (rmChar '\"' $ show m) ++ "\", shape=" ++ (flines!evshape) ++ ", fontcolor=" ++ (flines!loopcol)
                  LoopRcv -> (rmChar '\"' $ show m) ++ "\", shape=" ++ (flines!evshape) ++ ", fontcolor=" ++ (flines!loopcol)
                  _       -> error (msgFormat SGG "ERROR " ++ show (d, (s,r), m))
