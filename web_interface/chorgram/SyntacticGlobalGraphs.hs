--
-- Author: Emilio Tuosto <emilio@le.ac.uk>
--
-- This module contains function to project a syntactic choreography,
-- to calculate its semantics, and to output it in dot format.
--

module SyntacticGlobalGraphs where

import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import Misc
import CFSM
import DotStuff

-- A syntactic global graph is a set of nodes, a source, a sink, and a
-- set of edges We assume that cp's will be automatically generated
-- (uniquely) during parsing
data GG = Emp
        | Act Channel Message
        | Par [GG]
        | Bra (Set GG)
        | Seq [GG]
        | Rep GG Ptp
        deriving (Eq, Ord, Show)

type Endpoint = String

-- Syntactic global graphs can be normalised by flattening nested | and +
-- the name normGG is misleading. TODO: change normGG to preNormGG or flattenGG 
normGG :: GG -> GG
normGG gg = case gg of
             Emp       -> Emp -- note that this case should never happen for parsed graphs
             Act _ _   -> gg
             Par ggs   -> Par $ L.sort (normPar ggs)
             Bra ggs   -> let nb = normBra $ S.toList ggs in
                           (if (S.size nb==1) then (head $ S.toList nb) else (Bra nb))
             Seq ggs   -> Seq (L.map normGG ggs)
             Rep gg' p -> Rep (normGG gg') p
  where normPar gs = case gs of
                      []   -> []
                      g:l' -> case normGG g of
                               Par ggs' -> normPar (ggs' ++ l')
                               _        -> [normGG g] ++ (normPar l')
        normBra gs = case gs of
                      []   -> S.empty
                      g:l' -> case (normGG g) of
                               Bra ggs' -> normBra ((S.toList ggs') ++ l')
                               _        -> S.union (S.singleton $ normGG g) (normBra l')

-- start g g' checks if g is a prefix of g'
startGG :: GG -> GG -> Bool
startGG g g' = g == g' || case g' of
                            Seq ggs -> not (L.null ggs) && startGG g (head ggs)
                            Bra ggs -> not (L.null l) && L.all (startGG g) l
                                where l = S.toList ggs
                            _       -> False

--
-- factorise gg rewrites a GG in normal form by factorising the common
-- parts of branches
-- PRE: gg in normal form
-- POST: application of the congruenze law g;g1 + g;g2 = g;(g1+g2) from left to right
--
factorise :: GG -> GG
factorise gg = case gg of
                Emp         -> Emp
                Act (_,_) _ -> gg
                Par ggs     -> Par (L.map factorise ggs)
                Bra ggs     -> if S.null ggs
                               then Emp
                               else let prefix     = prefOf (S.elemAt 0 ggs)
                                        part       = S.partition (startGG prefix) ggs
                                        prefOf gg' = case gg' of
                                                      Seq ggs' -> if L.null ggs' then error $ show (S.elemAt 0 ggs) else head ggs'
                                                      _        -> gg'
                                        ggSet      = fst part
                                    in normGG (Bra (S.union (fact prefix ggSet) (rest $ snd part)))
                                       where fact prefix ggSet = if S.size ggSet == 1
                                                                 then ggSet
                                                                 else S.singleton (Seq [prefix, factorise (Bra (S.map suffOf ggSet))])
                                             suffOf gg'        = case gg' of
                                                                  Seq ggs' -> if L.length ggs' == 1 then Emp else Seq (tail ggs')
                                                                  _        -> Emp
                                             rest ggSet'       = if S.size ggSet' == 1
                                                                 then ggSet'
                                                                 else S.singleton $ factorise (Bra ggSet')
                Seq ggs     -> Seq (L.map factorise ggs)
                Rep gg' p   -> Rep (factorise gg') p

--
-- PRE: the input ggs are factorised
-- POST: returns True iff th list is well-branched
--        
wb :: Set GG -> Bool
wb ggs =
  let ps             = S.toList $ ggptp S.empty (Bra ggs)
      disjoint (x, y)= S.null $ S.intersection x y
      same (x, y)    = x == y
      firstActs p gg = case sevAt p gg of
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

sevAt :: Ptp -> GG -> GG
sevAt p gg = case gg of
               Emp               -> Emp
               act@(Act (s,r) _) -> if (s==p || r==p) then act else Emp
               Par ggs           -> Par (L.map (sevAt p) ggs)
               Bra ggs           -> Bra (S.map (sevAt p) ggs)
               Seq ggs           -> Seq (L.map (sevAt p) ggs)
               Rep gg' p'        -> Rep (sevAt p gg') p'
                      
-- ggptp computes the set of participants of a global graph
ggptp :: Set Ptp -> GG -> Set Ptp
ggptp ptps g = case g of
                Emp         -> ptps
                Act (s,r) _ -> S.union ptps (S.fromList [s,r])
                Par gs      -> S.union ptps (S.unions $ L.map (ggptp S.empty) gs)
                Bra gs      -> S.union ptps (S.unions $ S.toList (S.map (ggptp S.empty) gs))
                Seq gs      -> S.union ptps (S.unions (L.map (ggptp S.empty) gs))
                Rep g' p    -> S.union ptps (ggptp (S.singleton p) g')

--
-- PRE:  actions are well formed (wffActions) ^ q0 /= qe ^ p is a participant of gg
-- POST: the non-minimised projection of GG wrt p and a unique exiting state (it must always exist!)
-- n is a counter for fresh state generation
-- q0 and qe correspond to the entry and exit state, respectively
--
proj :: GG -> Ptp -> State -> State -> Int -> (CFSM, State)
proj gg p q0 qe n =
  let suf = show n
      tau = (Tau, (p,p), "")
      dm  = ( (S.fromList [q0,qe], q0, S.singleton tau, tautrx q0 qe) , qe )
      tautrx q1 q2 = if q1==q2 then S.empty else S.singleton (q1, tau, q2)
  in case gg of
      Emp         -> dm
      Act (s,r) m -> if (p/=s && p/=r)
                     then dm
                     else ( ((S.fromList [q0, qe]), q0, S.singleton c, (S.singleton (q0,c,qe))) , qe )
        where c = if (p == s) then (Send,(p,r),m) else (Receive,(s,p),m)
      Par ggs     -> ( replaceState (initialOf m) q0 ( S.union (S.singleton qe) (statesOf m ) ,
                                                       initialOf m ,
                                                       S.union (actionsOf m) (S.singleton tau) ,
                                                       (transitionsOf m)
                                                     )
                      , qe )
        where m   = replaceState qe' qe (cfsmProd $ L.map fst mps)
              qe' = L.foldr stateProd "" (L.map snd mps)
              mps = L.map (\g -> proj g p q0 qe n) ggs
      Bra ggs     -> ( replaceStates (\q -> q € [q0 ++ (show i) | i <- [1 .. (length mps)]]) q0 (states, q0, acts, trxs) , qe )
        where (states, acts, trxs) = L.foldl
                                       (\(x,y,z) m -> ( S.union x (statesOf m) ,
                                                        S.union y (actionsOf m) ,
                                                        S.union z (transitionsOf m) )
                                       )
                                       (S.singleton qe, S.singleton tau, S.empty)
                                       ms
              ggs'                 = L.zip (S.toList ggs) [1..S.size ggs]
              mps                  = L.map (\(g,i) -> proj g p (q0 ++ (show i)) qe n) ggs'
              (ms, _)              = (L.map fst mps, L.map snd mps)
      Seq ggs     -> ( replaceState qe' qe (states, q0, acts, trxs) , qe )
        where ( _ ,  qe' , states , acts , trxs ) =
                L.foldl
                  (\( i , qi , x , y , z ) g ->
                    let ( m , qf' ) = proj g p qi (qe ++ (show i)) n in
                     ( i+1 ,
                       qf' ,
                       S.union x (statesOf m) ,
                       S.union y (actionsOf m) ,
                       S.union z (transitionsOf m) )
                  )
                  ( 0 , q0 , S.empty , S.empty , S.empty )
                  ggs
              chain                    = [q0] ++ [q0 ++ "_" ++ (show i) | i <- [1 .. (length ggs)-1]] ++ [qe]
              aux z gs                 = case gs of
                                          [] -> []
                                          g:gs' -> (fst $ proj g p (head z) (head (tail z)) n) : (aux (tail z) gs')
      Rep g p'    -> if (S.member p repptps) then ( ggrep , qe' ) else dm
        where repptps        = ggptp S.empty g
              ggrep          = ( S.unions [statesOf body, statesOf loop, statesOf exit] ,
                                 initialOf body ,
                                 S.unions [actionsOf body, actionsOf loop, actionsOf exit] ,
                                 S.unions [transitionsOf body, transitionsOf loop, transitionsOf exit] )
              ( body , q )   = proj g p q0 (qe ++ suf) (n+2)
              ( loop' , ql ) = proj (helper (lpref ++ suf)) p q (q0 ++ suf) (n + 2)
              loop           = replaceState ql q0 loop'
              ( exit , qe' ) = proj (helper (epref ++ suf)) p q qe (n + 2)
              helper s       = Par (L.map (\p'' -> Act (p',p'') s) (S.toList $ S.delete p' repptps))

--
-- PRE:  actions are well formed (wffActions) ^ q0 /= qe ^ ps are all the participants of gg
-- POST: the system corresponding to GG wrt ps
-- n is a counter for fresh state generation
-- q0 and qe are used to generate  (entry and exit) nodes of each participant
-- Note that the resulting system is made of NON-MINIMISED machines
--
-- projAll :: GG -> [Endpoint] -> State -> State -> Int -> System
-- projAll gg ps q0 qe n = (L.map (\p -> fst $ proj gg (idxOffset p ps 0) mps (p++q0) (p++qe) n) ps, mps)
--     where mps = M.fromList [(p,ps!!p) | p <- [0 .. (L.length ps) - 1 ]]

-- DOT format

-- A representation for GG in dot
type PD = ([(Int,String)],[(Int,Int)])

--
-- gg2dot gg name transforms a GG in dot format
-- TODO: improve on fresh node generation
--
gg2dot :: GG -> String -> String -> String
gg2dot gg name nodeSize =
  let myshow n          = (if n < 0 then "_" else "") ++ (show $ abs n)
      header            = "digraph " ++ name ++ " {\n   node [width=" ++ nodeSize ++ ", height=" ++ nodeSize ++ "]\n\n"
      maxIdx vs         = aux vs 0
        where aux [] v = v+1
              aux ((v',_):vs') v = aux vs' (max v v')
      helper vs as gg_  = let (sink, i)       = (last vs, 1 + (maxIdx vs))
                              attach idx idx' = [(s,t) | (s,t) <- as, t /= fst sink] ++
                                                [(s,idx) | (s,t) <- as, t == (fst sink)] ++
                                                [(idx',fst sink)]
                              notgate         = \v -> v/=i && v/=(-i)
                          in case gg_ of
                               Emp      -> (vs,as)
                               Act _ _  -> ((init vs) ++ [( i , labelOf gg_ )] ++ [sink], attach i i)
                               Par ggs  -> ((init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
                                   where (vs', as') = unionsPD forkV joinV notgate i (rename (\v -> not (notgate v)) i graphs)
                                         graphs     = (L.map (helper [(i,forkV),(-i,joinV)] [(i,-i)]) ggs)
                               Bra ggs  -> ((init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
                                   where (vs', as') = unionsPD branchV mergeV notgate i (rename (\v -> not (notgate v)) i graphs)
                                         graphs     = S.toList (S.map (helper [(i,branchV),(-i,mergeV)] [(i,-i)]) ggs)
                               Seq ggs -> graphy ggs vs as
                                   where graphy ggs_ vs_ as_ = case ggs_ of
                                                                []       -> (vs_,as_)
                                                                gg':ggs' -> graphy ggs' vs'' as''
                                                                    where (vs0,as0)   = helper [(idx,""),(-idx,"")] [(idx,-idx)] gg'
                                                                          (vs',as')   = renameVertex notgate (vs0,as0) (1 + maxIdx vs0)
                                                                          (vs'',as'') = catPD (\(_,l) -> l /= "") (vs_,as_) (vs',as')
                                                                          idx         = 1 + maxIdx vs_
                               Rep gg' _ -> ((init vs) ++ vs' ++ [sink], (attach i (-i)) ++ ((-i,i):as'))
                                   where (vs', as') = helper [(i,mergeV),(-i,branchV)] [(i,-i)] gg'
        where rename excluded offset pds       = case pds of
                                                   (vs1,as1):pds' -> ([(newNode excluded v offset,l) | (v,l) <- vs1],
                                                                      [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as1]) :
                                                                     (rename excluded (1 + offset + maxIdx vs1) pds')
                                                   []             -> []
              unionsPD gl gl' included idx pds = ( [(idx,gl)] ++ [(v,l) | (v,l) <- L.concat  $ L.map fst pds, (included v)] ++ [((-idx),gl')],
                                                   L.concat $ L.map snd pds)
                                
      footer = "\n}\n"
      dotnodes vs = L.concat $ L.map (\(s,l) -> "\tnode" ++ (myshow s) ++ l) vs
      dotedges as = L.concat $ L.map (\(s,t) -> "\tnode" ++ (myshow s) ++ " -> node" ++ (myshow t) ++ "\n") as
      (vertexes, edges) = helper [(0,sourceV),(-1,sinkV)] [(0,-1)] gg
  in header ++ (dotnodes vertexes) ++ (dotedges edges) ++ footer

labelOf :: GG -> String
labelOf gg = case gg of
              Emp         -> sourceV
              Act (s,r) m -> " [label = \"" ++ s ++ " &rarr; " ++ r ++ " : " ++ m ++ "\", shape=rectangle, fontname=helvetica, fontcolor=MidnightBlue]"
              Par _       -> forkV
              Bra _       -> branchV
              Seq _       -> ""
              Rep _ _     -> ""

-- catPD (vs,as) (vs',as') appends (vs', as') attaching its
-- source to the nodes of (vs,as) entering the sink of (vs,as)
--
-- Pre: vs and vs' start and end with the source and target vertex of
-- the corresponding graph
--
-- Post: the result is the sequential composition of the graphs embedding
-- the second just before  the sink of (vs,as); the source of the resulting
-- graph is the source of (vs,as)
--
catPD :: ((Int,String) -> Bool) -> PD -> PD -> PD
catPD included ( vs, as ) ( vs' , as' ) = ( vs'' , as'' )
  where vs'' = (init vs) ++ (L.filter included vs') ++ [last vs]        
        as'' = [ ( n , m ) | ( n , m ) <- as, not(( n , m ) € maxs) ] ++
               [ ( n , m ) | ( n , _ ) <- maxs, ( _ , m ) <- min_   ] ++
               [ ( n , m ) | ( n , m ) <- as', not(( n , m ) € (min_ ++ max_)) ] ++
               [ ( n , fst $ last vs ) | ( n , _ ) <- max_  ]
        maxs = maxR as
        min_ = minR as'
        max_ = maxR as'

renameVertex :: (Int -> Bool) -> PD -> Int -> PD
renameVertex excluded ( vs , as ) offset = ([(newNode excluded s offset, l) | (s,l) <- vs],
                                            [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as])

