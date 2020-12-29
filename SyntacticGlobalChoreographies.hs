--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module contains function to project a syntactic g-choreography,
-- to calculate its semantics, and to output it in dot format.
--

module SyntacticGlobalChoreographies where

import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import Misc
import CFSM
import DotStuff
import Data.String.Utils(replace)

-- A syntactic global graph is a set of nodes, a source, a sink, and a
-- set of edges We assume that cp's will be automatically generated
-- (uniquely) during parsing
data GG = Emp
        | Act Channel Message
--        | LAct Channel Message
        | Par [GG]
        | Bra (Set GG)
        | Seq [GG]
        | Rep GG Ptp
    deriving (Eq, Ord, Show)

-- type Endpoint = String

normGG :: GG -> GG
--
-- Syntactic global graphs can be normalised by flattening nested |
-- and + the name normGG is misleading.
--
-- The function is based on the following structural rules:
-- (o) + (o) = (o)
-- ( GG, _|_, (o) ) abelian monoid
-- ( GG, _;_, (o) ) monoid
-- G + G' = G' + G
-- G;(G1 + G2) = G;G1 + G;G2
-- (G1 | G2) ; (G1' | G2') = G1;G1' | G2;G2'   if ptps(G2) \cap ptps(G1') = ptps(G1) \cap ptps(G2') = {}
--
-- the last equation is not applied yet
--
normGG gg =
  case gg of
    Seq ggs   -> let ngs = [g | g <- (L.map normGG ggs), g /= Emp]
                 in (if ngs ==[] then Emp else Seq ngs)
    Rep gg' p -> Rep (normGG gg') p
    Par ggs   -> let ngs = [g | g <- (normPar ggs), g /= Emp]
                 in (case ngs of
                        [] -> Emp
                        [g] -> g
                        _   -> Par $ L.sort ngs
                    )
    Bra ggs   -> let nb = S.filter (\g -> g /= Emp) (normBra $ S.toList ggs)
                 in (case S.size nb of
                        0 -> Emp
                        1 -> head $ S.toList nb
                        _ -> Bra nb
                    )
    _         -> gg
  where normPar gs = case gs of
                       []   -> []
                       [_]  -> gs
                       g:l' -> let ng = normGG g
                               in (case ng of
                                      Par ggs' -> normPar (ggs' ++ l')
                                      _        -> [ng] ++ (normPar l')
                                  )
        normBra gs = case gs of
                       []   -> S.empty
                       [g]  -> let ng = normGG g
                               in (if ng == Emp then S.empty else S.singleton ng)
                       g:l' -> let ng = normGG g
                               in (case ng of
                                      Bra ggs' -> normBra ((S.toList ggs') ++ l')
                                      _        -> S.union (S.singleton ng) (normBra l')
                                  )

startGG :: GG -> GG -> Bool
--
-- start g g' checks if g is a prefix of g'
--
startGG g g' = let ng = normGG g
                   ng' = normGG g'
               in (case ng' of
                      Seq ggs -> startGG ng (head ggs)
                      Bra ggs -> L.all (startGG ng) l
                        where l = S.toList ggs
                      _       -> False
                  )


factorise :: GG -> GG
--
-- factorise gg rewrites a GG in normal form by factorising the common
-- parts of branches
-- PRE : gg in normal form
-- POST: application of the congruenze law g;g1 + g;g2 = g;(g1+g2) from left to right
--
factorise gg =
  case gg of
    Emp       -> Emp
    Act _ _   -> gg
--    LAct _ _  -> gg
    Par ggs   -> Par (L.map factorise ggs)
    Bra ggs   ->
      if S.null ggs
      then Emp
      else
        let prefix     = prefOf (S.elemAt 0 ggs)
            part       = S.partition (startGG prefix) ggs
            prefOf gg' =
              case gg' of
                Seq ggs' -> if L.null ggs' then error $ show (S.elemAt 0 ggs) else head ggs'
                _        -> gg'
            ggSet      = fst part
        in normGG (Bra (S.union (fact prefix ggSet) (rest $ snd part)))
      where fact prefix ggSet =
              if S.size ggSet == 1
              then ggSet
              else S.singleton (Seq [prefix, factorise (Bra (S.map suffOf ggSet))])
            suffOf gg'        =
              case gg' of
                Seq ggs' -> if L.length ggs' == 1 then Emp else Seq (tail ggs')
                _        -> Emp
            rest ggSet'       =
              if S.size ggSet' == 1
              then ggSet'
              else S.singleton $ factorise (Bra ggSet')
    Seq ggs   -> Seq (L.map factorise ggs)
    Rep gg' p -> Rep (factorise gg') p


-- factoriseNew :: GG -> GG
-- --
-- -- factoriseNew gg rewrites a GG in normal form by factorising the common
-- -- parts of branches
-- -- PRE : gg in normal form
-- -- POST: application of the congruenze law g;g1 + g;g2 = g;(g1+g2) from left to right
-- --
-- factoriseNew gg = let ngg = normGG gg
--                   in (case ngg of
--                         Par ggs   -> Par (L.map factoriseNew ggs)
--                         Seq ggs   -> Seq (L.map factoriseNew ggs)
--                         Rep gg' p -> Rep (factoriseNew gg') p
--                         Bra ggs   -> let (seqs, o) = S.partition isSeq ggs
--                                          candidate = case (S.size seqs, S.size o) of
--                                                        (0, 0) -> error "Something very strange here"
--                                                        (_, 0) -> Seq (L.minimumBy (O.comparing L.length) ([l | Seq l <- S.toList seqs]))
--                                                        (0, _) -> ngg
--                                      in (if S.null seqs
--                                          then candidate
--                                          else combine (split candidate)
--                                         )
--                           where isSeq   = \_gg -> case _gg of Seq _ -> True; _ -> False
--                                 combine = \(p, s) -> normGG Seq [p, Bra s]
--                                 split   = \gc -> if check candidate seqs
--                                                  then 
--                         _         -> ngg
--                      )

wb :: Set GG -> Bool
--
-- PRE : the input ggs are factorised
-- POST: returns True iff th list is well-branched
--        
wb ggs =
  let ps             = S.toList $ gcptp S.empty (Bra ggs)
      disj (x, y) = S.null $ S.intersection x y
--      same (x, y)    = x == y
      firstActs p gg =
        case sevAt p gg of
          Emp               -> (S.empty, S.empty)
          act@(Act (s,_) _) ->
            if s == p
            then (S.singleton act, S.empty)
            else (S.empty, S.singleton act)
          -- act@(LAct (s,_) _) ->
          --   if s == p
          --   then (S.singleton act, S.empty)
          --   else (S.empty, S.singleton act)
          Par ggs'' ->
            L.foldr aux (S.empty, S.empty) ggs''
            where aux gg_ (fA,fP) =
                    let (fA',fP') = firstActs p gg_
                    in (S.union fA fA', S.union fP fP')
          Bra ggs'' ->
            S.foldr aux (S.empty, S.empty) ggs''
            where aux =
                    \gg_ (fA,fP) -> let (fA',fP') = firstActs p gg_ in ((S.union fA fA'), (S.union fP fP'))
          Seq ggs'' ->
            case ggs'' of
              []    -> (S.empty, S.empty)
              gg':_ ->
                if res == (S.empty, S.empty)
                then firstActs p (Seq (tail ggs''))
                else res
                where res = firstActs p gg'
          Rep gg' _        -> firstActs p gg'
      matrix         = M.fromList [(p, L.map (firstActs p) (S.toList ggs)) | p <- ps ]
      check prop f p = L.all (\pairs -> prop (f pairs)) (matrix!p)
      active         = S.fromList ([ p | p <- ps, check (\x -> not (S.null x)) fst p ])
      passive        = S.fromList ([ p | p <- ps, check (\x -> not (S.null x)) snd p ])
      getpairs l res =
        case l of
          []   -> res
          e:l' -> getpairs l' (res ++ [(e,e') | e' <- l'])
  in S.null ggs || (
                    L.all (\p -> check (\x -> S.null x) fst p || check (\x -> not (S.null x)) fst p) ps &&
                    L.all (\p -> check (\x -> S.null x) snd p || check (\x -> not (S.null x)) snd p) ps &&
                    (S.size active == 1) && (disj (active, passive)) &&
                    L.all disj (getpairs (L.map fst (matrix!(S.elemAt 0 active))) []) &&
                    L.all (\p -> L.all disj (getpairs (L.map snd (matrix!p)) [])) (S.toList passive)
                   )

sevAt :: Ptp -> GG -> GG
sevAt p gg =
  case gg of
    Emp                -> Emp
    act@(Act (s,r) _)  -> if (s==p || r==p) then act else Emp
--    act@(LAct (s,r) _) -> if (s==p || r==p) then act else Emp
    Par ggs            -> Par (L.map (sevAt p) ggs)
    Bra ggs            -> Bra (S.map (sevAt p) ggs)
    Seq ggs            -> Seq (L.map (sevAt p) ggs)
    Rep gg' p'         -> Rep (sevAt p gg') p'
                      
gcptp :: Set Ptp -> GG -> Set Ptp
--
-- gcptp computes the set of participants of a global graph
--
gcptp ptps g =
  case g of
    Emp          -> ptps
    Act (s,r) _  -> S.union ptps (S.fromList [s,r])
--    LAct (s,r) _ -> S.union ptps (S.fromList [s,r])
    Par gs       -> S.union ptps (S.unions $ L.map (gcptp S.empty) gs)
    Bra gs       -> S.union ptps (S.unions $ S.toList (S.map (gcptp S.empty) gs))
    Seq gs       -> S.union ptps (S.unions (L.map (gcptp S.empty) gs))
    Rep g' p     -> S.union ptps (gcptp (S.singleton p) g')

loopBack :: State -> State
loopBack q = "__l__" ++ q

loopExit :: State -> State
loopExit q = "__e__" ++ q

proj :: GG -> State -> State -> Ptp -> Int -> P -> (CFSM, State)
proj gc q0 qe p n pmap =
--
-- PRE : actions are well formed (wffActions) ^ q0 /= qe
--       p is a participant of gc
--       the participants of gc are all and only those in M.elems pmap
--       n controls the projection of loops:
--         * n >= 0 is the number of unfoldings
--         * n <  0 use normal projections without unfoldings
-- POST: the non-minimised projection of gc wrt p and a unique interface state (it must always exist!)
--       q0 must be the initial state of the resulting cfsm and qe is the interface state, respectively
--
  let g_loop =
        \s ptps -> Seq (L.map (\r -> Act (s,r) (loopBack q0)) (S.toList $ S.delete s ptps))
      g_exit =
        \s ptps -> Seq (L.map (\r -> Act (s,r) (loopExit qe)) (S.toList $ S.delete s ptps))
      onetr =
        \q1 q2 l -> (((S.fromList [q1, q2]), q1, S.singleton l, (S.singleton (q1, l, q2))), q2)
      inverse =
        inv pmap
      pTau =
        \l -> ((show $ inverse!p, show $ inverse!p), l, "")
      dm =
        \q_ l -> onetr q0 q_ (pTau l)
  in case gc of
    Emp -> dm qe Tau
    Act (s,r) m ->
      case (p==s,p==r) of
        (False, False) -> dm qe Tau
        _ -> onetr q0 qe label
          where label =
                  if p==s
                  then ((show $ inverse!p, show $ inverse!r), Send, m)
                  else ((show $ inverse!s, show $ inverse!p), Receive, m)
    Par ggs ->
      (replaceState (initialOf m) q0 m, qe)
      where m   = replaceState qe' qe (cfsmProd $ L.map fst mps)
            qe' = L.foldr stateProd "" (L.map snd mps)
            mps = L.map (\g -> proj g q0 qe p n pmap) ggs
    Bra ggs ->
      (replaceStates (\q_ -> q_ € [qe ++ (show i) | i <- [1 .. (length mps)]]) qe cfsm, qe)
      where (states, acts, trxs) =
              L.foldl
                (\(x,y,z) m -> (S.union x (statesOf m),
                                S.union y (actionsOf m),
                                S.union z (transitionsOf m)
                               )
                )
                (S.singleton qe, S.singleton $ pTau Tau, S.empty)
                (L.map fst mps)
            ggs' = L.zip (S.toList ggs) [1 .. S.size ggs]
            mps  = L.map (\(g,i) -> proj g (q0 ++ (show i)) (qe ++ (show i)) p n pmap) ggs'
            cfsm = replaceStates (\q_ -> q_ € [q0 ++ (show i) | i <- [1 .. (length mps)]]) q0 (states, q0, acts, trxs)
    Seq ggs ->
      ( replaceState qe' qe (states, q0, acts, trxs) , qe )
      where (_, qe', states, acts, trxs) =
                L.foldl
                  (\(i, qi, x, y, z) g ->
                    let (m, qf') = proj g qi (qe ++ (show i)) p n pmap
                    in
                     (i + 1,
                      qf',
                      S.union x (statesOf m),
                      S.union y (actionsOf m),
                      S.union z (transitionsOf m)
                     )
                  )
                  (0, q0, S.empty, S.empty, S.empty)
                  ggs
    Rep g p' ->
      if (S.member p ptpsloop) then (fst m, qe) else (dm qe Tau)
      where
        ptpsloop = gcptp S.empty g
        suf = if n<0 then show (-n) else show n
        m =
          if n >= 0
          then
            case (p == p' , n == 0) of
              (True, True) ->
                proj (g_exit p ptpsloop) q0 qe p n pmap
              (True, False) ->
                let (cfsm_exit, _) = proj (g_exit p ptpsloop) q0 qe p (n-1) pmap
                    (cfsm_loop, qel) = proj (g_loop p ptpsloop) q0 (qe ++ suf) p (n-1) pmap
                    (cfsm_body, qeb) = proj g qel (qe ++ "body" ++ suf) p (n-1) pmap
                    (cfsm_itr, qu) = proj (Rep g p) qeb (qe ++ "iter" ++ suf) p (n-1) pmap
                    cfsm_bra = cfsmUnion q0 [cfsm_exit, cfsm_loop]
                in (replaceState (qe ++ "iter" ++ suf) qe (cfsmUnion q0 [cfsm_bra, cfsm_body, cfsm_itr]), qu)
              (_, _) ->
                let (cfsm_loop, qel) = onetr q0 (qe ++ suf) ((show $ inverse!p', show $ inverse!p), Receive, (loopBack q0))
                    (cfsm_exit, _) = onetr q0 qe ((show $ inverse!p', show $ inverse!p), Receive, (loopExit qe))
                    (cfsm_body, _) = proj g qel q0 p (n-1) pmap
                in (cfsmUnion q0 [cfsm_loop, cfsm_exit, cfsm_body], qe)
          else
            if p == p'
            then
              let (cfsm_loop, qel) = proj (g_loop p ptpsloop) q0 (qe ++ suf) p n pmap
                  (cfsm_exit, _) = proj (g_exit p ptpsloop) q0 qe p n pmap
                  (cfsm_body, _) = proj g qel q0 p n pmap
              in (cfsmUnion q0 [cfsm_loop, cfsm_exit, cfsm_body], qe)
            else
              let (cfsm_loop, qel) = onetr q0 (qe ++ suf) ((show $ inverse!p', show $ inverse!p), Receive, (loopBack q0))
                  (cfsm_exit, _) = onetr q0 qe ((show $ inverse!p', show $ inverse!p), Receive, (loopExit qe))
                  (cfsm_body, _) = proj g qel q0 p (n-1) pmap
              in (cfsmUnion q0 [cfsm_loop, cfsm_exit, cfsm_body], qe)

projx :: Bool -> GG -> P -> Ptp -> State -> State -> Int -> (CFSM, State)
projx loopFlag gg pmap p q0 qe n =
--
-- PRE : actions are well formed (wffActions) ^ q0 /= qe ^ p is a participant of gg
-- POST: the non-minimised projection of gg wrt p and a unique exiting state (it must always exist!)
--       n is a counter for fresh state generation
--       q0 and qe correspond to the entry and exit state, respectively
--
-- Parameter 'loopFlag' is set to true when projecting loops
-- and to false otherwise.
--
  let inverse = inv pmap
      taul l = ((show $ inverse!p, show $ inverse!p), l, "")
      tautrx = \q1 q2 l ->  S.singleton (q1, taul l, q2)
      dm q l = ((S.fromList [q0, q], q0, S.singleton (taul l), tautrx q0 q l), q)
  in case gg of
      Emp -> if loopFlag then (dm (qe ++ "Break") Break) else (dm qe Tau)
      Act (s,r) m -> if (p /= s && p /= r)
                     then dm qe Tau
                     else (((S.fromList [q0, qe]), q0, S.singleton c, (S.singleton (q0, c, qe))), qe)
        where c = if (p == s)
                  then ((show $ inverse!p, show $ inverse!r), Send, m)
                  else ((show $ inverse!s, show $ inverse!p), Receive, m)
      -- LAct(s,r) m -> if (p/=s && p/=r)
      --                then dm qe Tau
      --                else (((S.fromList [q0, qe]), q0, S.singleton c, (S.singleton (q0, c, qe))), qe)
      --   where c = if (p == s)
      --             then ((show $ inverse!p, show $ inverse!r), LoopSnd, m)
      --             else ((show $ inverse!s, show $ inverse!p), LoopRcv, m)
      Par ggs -> (replaceState (initialOf m) q0 (S.insert qe (statesOf m),
                                                 initialOf m,
                                                 S.insert (taul Tau) (actionsOf m),
                                                 (transitionsOf m)
                                                ),
                   qe
                 )
        where m   = replaceState qe' qe (cfsmProd $ L.map fst mps)
              qe' = L.foldr stateProd "" (L.map snd mps)
              mps = L.map (\g -> projx loopFlag g pmap p q0 qe n) ggs
      Bra ggs     -> (replaceStates (\q -> q € [qe ++ (show i) | i <- [1 .. (length mps)]]) qe (replaceStates (\q -> q € [q0 ++ (show i) | i <- [1 .. (length mps)]]) q0 (states, q0, acts, trxs)), qe)
        where (states, acts, trxs) = L.foldl
                (\(x,y,z) m -> (S.union x (statesOf m),
                                S.union y (actionsOf m),
                                S.union z (transitionsOf m) )
                )
                (S.singleton qe, S.singleton $ taul Tau, S.empty)
                (L.map fst mps)
              ggs' = L.zip (S.toList ggs) [1 .. S.size ggs]
              mps  = L.map (\(g,i) -> projx loopFlag g pmap p (q0 ++ (show i)) (qe ++ (show i)) n) ggs'
      Seq ggs -> ( replaceState qe' qe (states, q0, acts, trxs) , qe )
        where (_, qe', states, acts, trxs) =
                L.foldl
                  (\(i, qi, x, y, z) g ->
                    let (m, qf') = projx loopFlag g pmap p qi (qe ++ (show i)) n in
                     (i + 1,
                      qf',
                      S.union x (statesOf m),
                      S.union y (actionsOf m),
                      S.union z (transitionsOf m)
                     )
                  )
                  (0, q0, S.empty, S.empty, S.empty)
                  ggs
      Rep g p' -> if (S.member p bodyptps) then (ggrep, qe') else (dm qe' Tau)
        where bodyptps    = gcptp S.empty g
              ggrep       = (S.unions [statesOf body, statesOf loop, statesOf exit],
                             initialOf body,
                             S.unions [actionsOf body, actionsOf loop, actionsOf exit],
                             S.unions [transitionsOf body, transitionsOf loop, transitionsOf exit]
                            )
              suf         = show n
              (body', q)  = projx True g pmap p q0 (qe ++ suf) (n + 2)
              breakPoints = S.map (\(_, _, q_) -> q_) (S.filter (\(_, (_, l, _), _) -> l == Break) (transitionsOf body'))
              body        = replaceStates (\q_ -> q_ € S.toList breakPoints) (qe ++ suf) body'
              (loop', ql) = projx False (helper (lpref ++ suf)) pmap p q (q0 ++ suf) (n + 2)
              loop        = replaceState ql q0 loop'
              (exit, qe') = projx False (helper (epref ++ suf)) pmap p q qe (n + 2)
--              helper msg  = Par (L.map (\p'' -> LAct (p',p'') msg) (S.toList $ S.delete p' bodyptps))
              helper msg  = Par (L.map (\p'' -> Act (p',p'') msg) (S.toList $ S.delete p' bodyptps))


-- projAll :: GG -> [Endpoint] -> State -> State -> Int -> System
-- projAll gg ps q0 qe n = (L.map (\p -> fst $ proj gg (idxOffset p ps 0) mps (p++q0) (p++qe) n) ps, mps)
--
-- PRE : actions are well formed (wffActions) ^ q0 /= qe ^ ps are all the participants of gg
-- POST: the system corresponding to GG wrt ps
-- n is a counter for fresh state generation
-- q0 and qe are used to generate  (entry and exit) nodes of each participant
-- Note that the resulting system is made of NON-MINIMISED machines
--
--     where mps = M.fromList [(p,ps!!p) | p <- [0 .. (L.length ps) - 1 ]]


-- Representing GGs in DOT format

-- The dot graph is a pair made of a list of nodes and a list of edges
type PD = ([(DotNode, DotString)], [(DotNode, DotNode)])

node2dot :: DotNode -> DotString
-- DOT representation of GG nodes
node2dot n = (if n < 0 then "_" else "") ++ (show $ abs n)
      
gc2dot :: GG -> String -> DotString -> DotString
--
-- gc2dot gg name transforms a GG in dot format
-- TODO: improve on fresh node generation
--
gc2dot gg name nodeSize =
  let maxIdx vs = aux vs 0
        where aux [] v = v + 1
              aux ((v', _):vs') v = aux vs' (max v v')
      dummyGG n = ([(n, branchV), (-n, mergeV)], [(n, -n)])
      helper vs as gg_  = let (sink, i)       = (L.last vs, 1 + (maxIdx vs))
                              attach idx idx' = [(s, t)   | (s, t) <- as, t /= fst sink] ++
                                                [(s, idx) | (s, t) <- as, t == fst sink] ++
                                                [(idx', fst sink)]
                              notgate         = \v -> v /= i && v /= (-i)
                          in case gg_ of
                               Emp      -> (vs, as)
                               Act _ _  -> ((L.init vs) ++ [(i, dotLabelOf gg_ )] ++ [sink], attach i i)
--                               LAct _ _ -> ((L.init vs) ++ [(i, dotLabelOf gg_ )] ++ [sink], attach i i)
                               Par ggs  -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
                                   where (vs', as') = unionsPD forkV joinV notgate i (rename (\v -> not (notgate v)) i graphs)
                                         graphs     = (L.map (helper [(i,forkV),(-i,joinV)] [(i,-i)]) ggs)
                               Bra ggs  -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
                                   where (vs', as') = unionsPD branchV mergeV notgate i (rename (\v -> not (notgate v)) i graphs)
                                         (evs, eas) = dummyGG i
                                         graphs     = S.toList (S.map (helper evs eas) ggs)
                               Seq ggs -> graphy ggs vs as
                                   where graphy ggs_ vs_ as_ = case ggs_ of
                                                                []       -> (vs_,as_)
                                                                Emp:ggs' -> graphy ggs' vs_ as_
                                                                gg':ggs' -> graphy ggs' vs'' as''
                                                                    where (vs0,as0)   = helper [(idx,""),(-idx,"")] [(idx,-idx)] gg'
                                                                          (vs',as')   = renameVertex notgate (vs0,as0) (1 + maxIdx vs0)
                                                                          (vs'',as'') = catPD (\(_,l) -> l /= "") (vs_,as_) (vs',as')
                                                                          idx         = 1 + maxIdx vs_
                               Rep gg' _ -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ ((-i,i):as'))
                                   where
                                     (evs, eas) = dummyGG i
                                     (vs', as') = helper evs eas gg'
        where rename excluded offset pds
                = case pds of
                    (vs1, as1):pds' -> ([(newNode excluded v offset,l) | (v,l) <- vs1],
                                        [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as1]) :
                                       (rename excluded (1 + offset + maxIdx vs1) pds')
                    []             -> []
              unionsPD gl gl' included idx pds
                = ([(idx,gl)] ++ [(v,l) | (v,l) <- L.concat  $ L.map fst pds, (included v)] ++ [((-idx),gl')],
                    L.concatMap snd pds)
      dotnodes vs  = L.concatMap (\(s, l) -> "\tnode" ++ (node2dot s) ++ l) vs
      dotedges as  = L.concatMap (\(s, t) -> "\tnode" ++ (node2dot s) ++ " -> node" ++ (node2dot t) ++ "\n") as
      (evs_, eas_) = ([(1, sourceV), (-1, sinkV)], [(1, -1)])
      (vertexes, edges) = helper evs_ eas_ gg
      (header,  footer) = ("digraph " ++ name ++ " {\n   node [width=" ++ nodeSize ++ ", height=" ++ nodeSize ++ "]\n\n", "\n}\n")
  in header ++ (dotnodes vertexes) ++ (dotedges edges) ++ footer

dotLabelOf :: GG -> DotString
dotLabelOf gg = case gg of
              Emp         -> sourceV
              Act (s,r) m -> " [label = \"" ++ s ++ " &rarr; " ++ r ++ " : " ++ m ++ "\", shape=rectangle, fontname=helvetica, fontcolor=MidnightBlue]\n"
--              LAct(s,r) m -> dotLabelOf (Act (s,r) m)
              Par _       -> forkV
              Bra _       -> branchV
              Seq _       -> ""
              Rep _ _     -> ""

catPD :: ((DotNode, DotString) -> Bool) -> PD -> PD -> PD
--
-- catPD (vs,as) (vs',as') appends (vs', as') attaching its
-- source to the nodes of (vs,as) entering the sink of (vs,as)
--
-- PRE : vs and vs' start and end with the source and target vertex of
--       the corresponding graph
--
-- POST: the result is the sequential composition of the graphs
--       embedding the second just before the sink of (vs,as); the
--       source of the resulting graph is the source of (vs,as)
--
catPD included (vs, as) (vs', as') = (vs'', as'')
  where vs'' = (L.init vs) ++ (L.filter included vs') ++ [L.last vs]        
        as'' = [ (n, m) | (n, m) <- as, not((n, m) € maxs) ] ++
               [ (n, m) | (n, _) <- maxs, (_, m) <- min_   ] ++
               [ (n, m) | (n, m) <- as', not((n, m) € (min_ ++ max_)) ] ++
               [ (n, fst $ L.last vs) | (n, _) <- max_  ]
        maxs = maxR as
        min_ = minR as'
        max_ = maxR as'

renameVertex :: (DotNode -> Bool) -> PD -> DotNode -> PD
renameVertex excluded (vs, as) offset = ([(newNode excluded s offset, l) | (s,l) <- vs],
                                         [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as])


--
-- Stuff to generate global graphs in the gml format
--
data GMLTAGS = Source | Sink | Fork | Branch | Join | Merge | Loop
  deriving (Eq, Ord, Show)

gmlstyle :: GMLTAGS -> String
-- using the yed style
gmlstyle tag = ""

gmldata :: String -> String -> String
gmldata k v = "      <data key=\"" ++ k ++ "\">" ++ v ++ "</data>\n"

gmlLabelOf :: GG -> String
gmlLabelOf gg = case gg of
              Act (s,r) m -> (gmldata "sender" s ++ gmldata "receiver" r ++ gmldata "payload" m)
--                ++ "      <data key=\"ylabel\">\n        <y:ShapeNode>\n          <y:Geometry height=\"30.0\" width=\"30.0\" x=\"63.0\" y=\"322.0\"/>\n          <y:Fill color=\"#FFCC00\" transparent=\"false\"/>\n          <y:BorderStyle color=\"#000000\" raised=\"false\" type=\"line\" width=\"1.0\"/>\n          <y:NodeLabel alignment=\"center\" autoSizePolicy=\"content\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"plain\" hasBackgroundColor=\"false\" hasLineColor=\"false\" height=\"17.96875\" horizontalTextPosition=\"center\" iconTextGap=\"4\" modelName=\"custom\" textColor=\"#000000\" verticalTextPosition=\"bottom\" visible=\"true\" width=\"140.0\" x=\"-5.9052734375\" xml:space=\"preserve\" y=\"6.015625\">" ++  (show s) ++ " -> " ++ (show r) ++ " : " ++ (show m) ++ "<y:LabelModel><y:SmartNodeLabelModel distance=\"4.0\"/></y:LabelModel><y:ModelParameter><y:SmartNodeLabelModelParameter labelRatioX=\"0.0\" labelRatioY=\"0.0\" nodeRatioX=\"0.0\" nodeRatioY=\"0.0\" offsetX=\"0.0\" offsetY=\"0.0\" upX=\"0.0\" upY=\"-1.0\"/></y:ModelParameter></y:NodeLabel>\n          <y:Shape type=\"rectangle\"/>\n        </y:ShapeNode>\n      </data>\n"
--              LAct(s,r) m -> gmlLabelOf (Act (s,r) m)
              _       -> ""

gmlNode :: String -> Int -> String
gmlNode d idn  = "    <node id=\"" ++ (show idn) ++ "\">\n" ++ d ++ "    </node>\n"

gmlEdge :: Int -> Int -> String
gmlEdge source target = "    <edge source=\"" ++ (show source) ++ "\" target=\"" ++ (show target) ++ "\"></edge>\n"

gmlOpenGate :: Int -> GMLTAGS -> String
gmlOpenGate idn gate = gmlNode (gmldata "open" (show gate)) idn

gmlCloseGate :: Int -> GMLTAGS -> String
gmlCloseGate idn gate = gmlNode (gmldata "close" (show gate)) (-idn)

gmlrename :: (Int -> Bool) -> Int -> Int -> String -> String
gmlrename excluded n j s = if (excluded n)
                           then s
                           else let idn = (if n > 0 then n + j else n - j)
                                in replace ("<node id=\"" ++ (show n)) ("<node id=\"" ++ (show idn)) s

gc2graphml :: GG -> String
--
-- gc2dot gg name transforms a GG in dot format
-- TODO: improve on fresh node generation
--
gc2graphml gg =
  let header   = --"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:java=\"http://www.yworks.com/xml/yfiles-common/1.0/java\" xmlns:sys=\"http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0\" xmlns:x=\"http://www.yworks.com/xml/yfiles-common/markup/2.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:y=\"http://www.yworks.com/xml/graphml\" xmlns:yed=\"http://www.yworks.com/xml/yed/3\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd\">\n"
        "<?xml version=\'1.0\' encoding=\'utf-8\'?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"
      ogate    = "  <key attr.name=\"open\" attr.type=\"string\" for=\"node\" id=\"open\" />\n"
      cgate    = "  <key attr.name=\"close\" attr.type=\"string\" for=\"node\" id=\"close\" />\n"
      sender   = "  <key attr.name=\"sender\" attr.type=\"string\" for=\"node\" id=\"sender\" />\n"
      receiver = "  <key attr.name=\"receiver\" attr.type=\"string\" for=\"node\" id=\"receiver\" />\n"
      payload  = "  <key attr.name=\"payload\" attr.type=\"string\" for=\"node\" id=\"payload\" />\n"
      source   = ""--  <key attr.name=\"source\" attr.type=\"string\" for=\"node\" id=\"source\" />\n"
--      sink     = ""--"  <key attr.name=\"sink\" attr.type=\"string\" for=\"node\" id=\"sink\" />\n"
      yattr    = ""--  <key for=\"node\" id=\"ylabel\" yfiles.type=\"nodegraphics\"/>"
      edir     = "  <graph edgedefault=\"directed\">\n"
      footer   = "  </graph>\n</graphml>\n"
      maxIdx vs = aux vs 0
        where aux [] v = v + 1
              aux ((v', _):vs') v = aux vs' (max v v')
      dummyGG n = ([(n, gmlOpenGate n Loop), (-n, gmlCloseGate n Loop)], [(n, -n)])
      helper vs as gg_  = let (sink, i) = (L.last vs, 1 + (maxIdx vs))
                              attach idx idx' = [(s, t)   | (s, t) <- as, t /= fst sink] ++
                                                [(s, idx) | (s, t) <- as, t == fst sink] ++
                                                [(idx', fst sink)]
                              notgate = \v -> v /= i && v /= (-i)
                          in case gg_ of
                               Emp -> (vs, as)
                               Act _ _  -> ((L.init vs) ++ [(i, gmlNode (gmlLabelOf gg_) i)] ++ [sink], attach i i)
--                               LAct _ _ -> ((L.init vs) ++ [(i, gmlNode (gmlLabelOf gg_) i)] ++ [sink], attach i i)
                               Par ggs -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
                                   where (vs', as') = gather (gmlOpenGate i Fork)
                                                             (gmlCloseGate i Join)
                                                             notgate
                                                             i
                                                             (rename (not.notgate) i graphs)
                                         graphs = (L.map (helper [(i, (gmlOpenGate i Fork)), ((-i), (gmlCloseGate i Join))] [(i, (-i))]) ggs)
                               Bra ggs -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
                                   where (vs', as') = gather (gmlOpenGate i Branch)
                                                             (gmlCloseGate i Merge)
                                                             notgate
                                                             i
                                                             (rename (not.notgate) i graphs)
                                         graphs = S.toList (S.map (helper [(i, (gmlOpenGate i Branch)), ((-i), (gmlCloseGate i Merge))] [(i, (-i))]) ggs)
                               Seq ggs -> graphy ggs vs as
                                   where graphy ggs_ vs_ as_ = case ggs_ of
                                                                []       -> (vs_,as_)
                                                                Emp:ggs' -> graphy ggs' vs_ as_
                                                                gg':ggs' -> graphy ggs' vs'' as''
                                                                    where (vs0,as0) = helper [(idx,""),(-idx,"")] [(idx,-idx)] gg'
                                                                          (vs',as') = renameVertex notgate (vs0,as0) (1 + maxIdx vs0)
                                                                          (vs'',as'') = catPD (\(_,l) -> l /= "") (vs_, as_) (vs', as')
                                                                          idx = 1 + maxIdx vs_
                               Rep gg' _ -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ ((-i,i):as'))
                                   where (evs, eas) = dummyGG i
                                         (vs', as') = helper evs eas gg'
        where rename excluded offset pds =
                case pds of
                  [] -> []
                  (vs1, as1):pds' -> ([(newNode excluded v offset, gmlrename excluded v offset l) | (v,l) <- vs1],
                                      [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as1]) : (rename excluded (1 + offset + maxIdx vs1) pds')
              gather gl gl' included idx pds =
                ([(idx,gl)] ++ [(v,l) | (v,l) <- L.concat  $ L.map fst pds, (included v)] ++ [((-idx),gl')],
                  L.concatMap snd pds)
      gmlNodes vs  = L.concatMap (\(_, s) -> s) vs
      gmlEdges as  = L.concatMap (\(s, t) -> gmlEdge s t) as
      (evs_, eas_) = ([(1, (gmlOpenGate 1 Source)), (-1, gmlCloseGate 1 Sink)], [(1, -1)])
      (vertexes, edges) = helper evs_ eas_ gg
  in header ++
     ogate ++
     cgate ++
     sender ++
     receiver ++
     payload ++
     source ++
--     sink ++
     yattr ++
     edir ++
     (gmlNodes vertexes) ++
     (gmlEdges edges) ++
     footer
