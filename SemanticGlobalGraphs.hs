--
-- Author: Emilio Tuosto <emilio@le.ac.uk>
--
-- This module implements the semantics defined in ICE16
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

--
-- Types for control points and events with the assumption that
-- (i,(Tau,(i,i),m)) is a "control point" event for branch
-- if m == "+" and for fork if m == "|"
--
type K = Int
type E = (K, Maybe Action)

--
-- A type for bijections on control points; we simply assume
-- that a mu is a control point (whose the corresponding merge/join
-- is -mu)
--
type Mu = K

--
-- Hypergraphs are sets of hyperedges on events, but we keep
-- computing also its min, max, fst, and lst sets: ie an HG
-- has the form (R,min,max,fst,lst)
--
type HE = (Set E, Set E)
type HG = (Set HE, Set E, Set E, Set HE, Set HE)


ptpsOf :: E -> Maybe (Ptp, Ptp)
ptpsOf (_, Just (_,(s,r),_)) = Just (s,r)
ptpsOf (_, Nothing)          = Nothing

sbjOf :: E -> Maybe Ptp
sbjOf (_, Nothing)                  = Nothing
sbjOf (_, Just (Tau, _, _))         = Nothing
sbjOf (_, Just (Send, (s,_), _))    = Just s
sbjOf (_, Just (Receive, (_,r), _)) = Just r
sbjOf (_, Just (LoopSnd, (s,_), _)) = Just s
sbjOf (_, Just (LoopRcv, (_,r), _)) = Just r

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

eventsOf :: Set HE -> [E]
eventsOf rel = S.foldr (\l l' -> l++l') [] set
  where set = S.map (\(es,es')->((S.toList es) ++ (S.toList es'))) rel
  
-- Some auxiliary functions implementing those defined in the JLAMP17 paper

cp :: E -> K
cp = fst

actOf :: E -> Maybe Action
actOf = snd

sbj :: E -> Maybe Ptp
sbj (_, Just (d,(s,r),_)) = case d of
                             Send    -> Just s
                             Receive -> Just r
                             LoopSnd -> Just s
                             LoopRcv -> Just r
                             Tau     -> Nothing
sbj (_, Nothing) = Nothing

cs :: HE -> Set E
cs = fst

ef :: HE -> Set E
ef = snd

-- # is not used; in JLAMP17 it is detoted as $\circ$
(#) :: HG -> HG -> HG
hg # hg' = ( rel, minOf hg, maxOf hg', fstOf hg, lstOf hg' )
  where rel =  S.fromList [ (fst es, snd es') | es  <- (S.toList $ relOf hg),
                                                es' <- (S.toList $ relOf hg'),
                                                (Misc.intersect (snd es) (fst es'))
                          ]

-- rmEvs hg evs returns the hypergraph obtained by restricting hg to events not in evs
rmEvs :: HG -> Set E -> HG
rmEvs hg = ( S.fromList rel, S.fromList min, S.fromList max, fst, lst )
  where
    evs' = [ e | e <- eventsOf hg, not (S.member e evs) ]
    rel  = [(e,e') | e <- comms, e' <- comms, precHG hg (e,e') ]
    min  = [e | e <- comms, L.all (\e' -> e==e' || not(precHG hg (e', e))) comms]
    max  = [e | e <- comms, L.all (\e' -> e==e' || not(precHG hg (e, e'))) comms]
    fst  = S.fromList [(e,e') | (e, e') <- rel, e € min ]
    lst  = S.fromList [(e,e') | (e, e') <- rel, e' € max ]



--
-- seqHG corresponds to seq in JLAMP17, but it considers only the
-- dependencies induces by lstOf and fstOf respectively
--
seqHG :: HG -> HG -> HG
seqHG hg hg' = ( rel, minOf hg, maxOf hg', S.union (fstOf hg) fst', S.union (lstOf hg') lst' )
  where rel = S.unions [relOf hg, relOf hg', S.fromList l]
        l   = [( S.singleton e, S.singleton e' ) |
               e  <- eventsOf $ lstOf hg,  isJust $ actOf e,
               e' <- eventsOf $ fstOf hg', isJust $ actOf e',
               (sbj e == sbj e')]
        fst' = S.fromList [h | h <- l, not(S.null (S.intersection (cs h) (S.unions $ S.toList (S.map cs (fstOf hg)))))]
        lst' = S.fromList [h | h <- l, not(S.null (S.intersection (ef h) (S.unions $ S.toList (S.map ef (lstOf hg)))))]

-- reflexive and transitive closure of the order induced by a hg
precHG :: HG -> (E, E) -> Bool
precHG hg ( e, e' ) = (e==e') || (reach (S.singleton e) e' (hb hg))

-- reach es e' ker returns true if e is reachable from one of the events in es
reach :: Set E -> E -> Set (E, E) -> Bool
reach es e' ker = not(S.null es) && ( e==e' || reach (S.union es' (stepHG e ker)) e' ker )
  where ( e, es' ) = (head esl, S.fromList $ tail esl)
        esl        = S.toList es
                                
stepHG :: E -> Set (E, E) -> Set E
stepHG e ker = S.map snd (S.filter (\(e',_) -> e == e') ker)

-- happens-before relation; in the paper this is the $\hghb$ map
hb :: HG -> Set (E,E)
hb ( rel, _, _, _, _ ) = S.fromList [ (e,e') | (es,es') <- S.toList rel, e <- S.toList es, e' <- S.toList es' ]

-- ws checks that two HG can be composed sequentially
ws :: HG -> HG -> Bool
ws hg hg' = L.all (precHG (seqHG hg hg')) chkl
  where chkl = [( e, e' ) | e  <- L.concat $ S.toList (S.map (S.toList . cs) (lstOf $ hg)),
                            e' <- L.concat $ S.toList (S.map (S.toList . ef) (fstOf $ hg'))]

hgwb :: [HG] -> [HG]
hgwb [] = []
hgwb hg:hgs =
  case rmEvs hg () of
    [] -> []
    _  -> let min = fstOf hg

--
-- PRE: 
-- POST: returns True iff the set ggs is made of well-branched graphs
--        
wb :: (Map String GG) -> Set GG -> Bool
wb env ggs =
  let ps             = S.toList $ ggptp env S.empty (Bra ggs)
      disjoint (x,y) = S.null $ S.intersection x y
      firstActs p gg = case hgAt env p gg of
                        Emp               -> (S.empty, S.empty)
                        act@(Act (s,_) _) -> if s == p
                                             then (S.singleton act, S.empty)
                                             else (S.empty, S.singleton act)
                        Par ggs''         -> L.foldr aux (S.empty, S.empty) ggs''
                            where aux gg_ (fA,fP) = let (fA',fP') = firstActs p gg_ in
                                                    (S.union fA fA', S.union fP fP')
                        Bra ggs''         -> S.foldr aux (S.empty, S.empty) ggs''
                            where aux = \gg_ (fA,fP) -> let (fA',fP') = firstActs p gg_ in
                                                        ((S.union fA fA'), (S.union fP fP'))
                        Seq ggs''         -> case ggs'' of
                                              []    -> (S.empty, S.empty)
                                              gg':_ -> if res == (S.empty, S.empty)
                                                       then firstActs p (Seq (tail ggs''))
                                                       else res
                                                           where res = firstActs p gg'
                        Rep gg' _        -> firstActs p gg'
                        Inv gname        -> firstActs p (env!gname)
      matrix         = M.fromList [(p, L.map (firstActs p) (S.toList ggs)) | p <- ps ]
      check prop f p = L.all (\pairs -> prop (f pairs)) (matrix!p)
      active         = S.fromList ([ p | p <- ps, check (\x -> not (S.null x)) fst p ])
      passive        = S.fromList ([ p | p <- ps, check (\x -> not (S.null x)) snd p ])
      getpairs l res = case l of
                        []   -> res
                        e:l' -> getpairs l' (res ++ [(e,e') | e' <- l'])
  in S.null ggs || (
                    L.all (\p -> check (\x -> S.null x) fst p || check (\x -> not (S.null x)) fst p) ps &&
                    L.all (\p -> check (\x -> S.null x) snd p || check (\x -> not (S.null x)) snd p) ps &&
                    (S.size active == 1) && (disjoint (active, passive)) &&
                    L.all disjoint (getpairs (L.map fst (matrix!(S.elemAt 0 active))) []) &&
                    L.all (\p -> L.all disjoint (getpairs (L.map snd (matrix!p)) [])) (S.toList passive)
                   )

unionHG :: Maybe HG -> Maybe HG -> Maybe HG
unionHG hg hg' 
  | (isJust hg) && (isJust hg') = let (hg1,hg1') = (fromJust hg, fromJust hg')
                                  in Just ( S.union (relOf hg1) (relOf hg1'),
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

-- p-only part of a graph; note this is cmputed on the graph, not on its semantics
hgAt :: (Map String GG) -> Ptp -> GG -> GG
hgAt env p gg =
  case gg of
    Emp               -> Emp
    act@(Act (s,r) _) -> if (s==p || r==p) then act else Emp
    Par ggs           -> Par (L.map (hgAt env p) ggs)
    Bra ggs           -> Bra (S.map (hgAt env p) ggs)
    Seq ggs           -> Seq (L.map (hgAt env p) ggs)
    Rep gg' p'        -> Rep (hgAt env p gg') p'
    Inv gname         -> hgAt env p (env!gname)

semList :: (Map String GG) -> Mu -> [GG] -> P -> (Mu,[Maybe HG])
semList env mu ggs ptps =
  case ggs of
    []        -> (mu,[])
    (gg:ggs') -> (mu'', ([Just hg'] ++ rest))
      where ( mu', hg' )   = sem env mu gg ptps
            ( mu'', rest ) = semList env mu' ggs' ptps

--
-- The semantic function {|_|} defined in the equations (3-7) of
-- JLAMP17 plus the treatment of iteration. It is assumed that some
-- syntactic checks are performed before the invocation (eg, in
--       Rep g' p
-- we assume p is one of the participants in g'. The labels of the
-- events correspond to those used in proj (again this is the case for
-- iteration).
--
sem :: (Map String GG) -> Mu -> GG -> P -> (Mu, HG)
sem env mu gg ptps =
  case gg of
   Emp -> ( mu, emptyHG )
   Act (s,r) m -> ( i, ( rel, e, e', rel, rel ) )
       where i   = 1 + mu
             e   = S.singleton ( i, Just ( Send, ( s, r ), m ) )
             e'  = S.singleton ( i, Just ( Receive, ( s, r ), m ) )
             rel = S.singleton ( e, e' )
   Par ggs -> if isJust hgu
              then (mu', fromJust hgu)
              else error (msgFormat SGG "Something wrong in a fork: " ++ (show (Par ggs)))
     where ( mu', l ) = semList env mu ggs ptps
           hgu        = unionsHG l
   Seq ggs -> case ggs of
               []            -> ( mu, emptyHG )
               [g']          -> sem env mu g' ptps
               gg':gg'':ggs' -> if (ws hg hg')
                                then hgs
                                else error (msgFormat SGG "Violation of well-sequencedness: " ++ show (Seq ggs))
                 where hgs           = (mu'', (seqHG hg hg'))
                       ( mu', hg )   = sem env mu gg' ptps
                       ( mu'', hg' ) = sem env mu' (Seq (gg'':ggs')) ptps
   Bra ggs -> ( i, fromJust hg' )
     where ( mu', l ) = semList env mu (S.toList ggs) ptps
           i          = 1 + mu'
           hgu        = unionsHG l
           hg'        = if (wb env ggs && isJust hgu)
                        then unionHG hgu
                             (Just ( S.fromList $ L.concat $ L.map helper l, e, e', fstOf $ fromJust hgu, lstOf $ fromJust hgu ))
                        else error (msgFormat SGG "Violation of well-branchedness: " ++ show (Bra ggs))
           e          = S.singleton (i, Nothing)
           e'         = S.singleton ((-i), Nothing)
           helper x   = if isJust x then let x' = fromJust x in [(e, minOf x')] ++ [(maxOf x', e')] else error (msgFormat SGG "ERROR ...")
   Rep gg' p -> ( mu', hgr )
     where ps                = ggptp env S.empty gg'
           ( mu', hgb )     = if S.member p ps then sem env mu gg' ptps else error (msgFormat SGG "Participant " ++ p ++ " is not in the loop: " ++ show (Rep gg' p))
           ( i, suf )       = ( 1+mu' , show i )
           ( eL, eE )       = (S.singleton (i, Just ( LoopSnd , ( p , p ) , lpref ++ suf )), S.singleton ((-i), Just ( LoopRcv , ( p , p ) , epref ++ suf )))
           rel              = S.fromList ([( S.singleton e , eE ) | e <- S.toList $ maxOf $ hgb] ++
                                          [( eL , S.singleton e ) | e <- S.toList $ minOf $ hgb] ++
                                          [( eE , eL )]
                                         )
           hgr              = (S.union rel (relOf hgb), eL, eE, (fstOf hgb), (lstOf hgb))
   Inv gname -> sem env mu (env!gname) ptps

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
                     ( Send, _, _ )    -> "snd"
                     ( Receive, _, _ ) -> "rcv"
                     ( LoopSnd, _, _ ) -> subjectOf act
                     ( LoopRcv, _, _ ) -> subjectOf act
                     _                 -> error (msgFormat SGG "ERROR " ++ show ev)

ev2dot :: E -> Map String String -> String
ev2dot ev@(_, Nothing) _  = cp2dot ev ++ cpV
ev2dot ev@(_, Just (d, (s,r), m)) flines =
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
