--
-- Authors: Emilio Tuosto <emilio@le.ac.uk>
--
-- This module implements the k-bounded semantis
--

module TS where

import CFSM
import Data.Set as S
import Data.List as L
import Misc
import DotStuff
import Data.Maybe
import Data.Map.Strict as M
import Data.Foldable as F

type Node          = [State]
type KEvent        = (State, State, Ptp, Ptp, Dir, Message)
type Interaction   = (Ptp, Ptp, Message)

type Buffer        = Map Channel [Message]
type Configuration = (Node, Buffer)
type KTrans        = Atrans Configuration KEvent
type TSb           = Agraph Configuration KEvent
type BowTie        = Set (KEvent, KEvent)

data Cause vertex label = Bp Ptp vertex -- Bp p q reads: branch of state q of machine p not represented 
                        | Rp vertex label label String -- Rp n e1 e2 "some txt" reads: branching e1 e2 from n violated because ++ "some text"
                        deriving ( Show )

isEmpty :: TSb -> Bool
isEmpty (confs, _, _, trans) = (S.size confs < 2) && (S.null trans)

noSelfLoop :: TSb -> Bool
noSelfLoop (_, _, _, trans) = S.null $ S.filter (\(x,_,z) -> x == z) trans

emptyBuffer :: P -> Buffer
emptyBuffer ptps = M.fromList [((s,r),[]) | (_,s) <- M.assocs ptps, (_,r) <- M.assocs ptps, s /= r]

-- toEvent c t
--   PRE:  t is a configuration enabled at c
--   POST: the event corresponding to t from c
toKEvent :: Configuration -> P -> LTrans -> KEvent
toKEvent (n, _) ptps (_, (d, (s,r), msg), _) = (n!!(findId s l), n!!(findId r l), s, r, d, msg)
  where l = M.assocs ptps

evt2interaction :: KEvent -> Interaction
evt2interaction (_, _, s, r, _, msg) = (s, r, msg)

action2interaction :: Action -> Interaction
action2interaction (_, (_, m'), msg) = (m',m',msg)

succEvents :: TSb -> Configuration -> Set KEvent
succEvents ts n = S.map (\(_, e, _) -> e) (deriv n ts)

reinit :: TSb -> Configuration -> TSb
reinit (states, _, events, trans) q0 = (nodes, q0, nevents, ntrans)
    where nodes   = S.fromList $ reachableNode (states, q0, events, trans) q0
          ntrans  = S.filter (\(n, _, n') -> S.member n nodes && S.member n' nodes) trans
          nevents = S.fromList [e | (_, e, _) <- S.toList ntrans]
sender :: KEvent -> Ptp
sender (_, _, s, _, _, _) = s

receiver :: KEvent -> Ptp
receiver ( _, _, _, r, _, _ ) = r

machines :: KTrans -> Set Ptp
machines ( _,e,_ ) = S.insert (receiver e) (S.singleton $ sender e)

deriv :: Configuration -> TSb -> Set KTrans
deriv n = \(_, _, _, trans) -> (S.filter (\(n', _, _) -> n == n') trans)

succConfs :: TSb -> Configuration -> Set Configuration
succConfs ts n = S.map (\(_, _, n') -> n') (deriv n ts)

succConf :: TSb -> Configuration -> KEvent -> Configuration
succConf ts n e = head $ S.toList $ S.map (\(_, _, n') -> n') (S.filter (\(_, e', _) -> e'==e) (deriv n ts))

project :: KEvent -> Ptp -> Maybe Action
project (_, _, s, r, _, msg) p = if s == p
                                 then Just (Send, (s,r), msg)
                                 else if r == p
                                      then Just (Receive, (s,r) , msg)
                                      else Nothing

projectTS :: TSb -> Ptp -> CFSM
projectTS (nodes, initnode, _, trans) p = (states, q0, actions, finalTrans)
    where states                      = S.insert q0 $ S.fold (S.union) S.empty $ S.map (\(x,_,z) -> S.insert x (S.singleton z)) finalTrans
          q0                          = nodelabel (fst initnode) ""
          actions                     = S.map (\(_,y,_) -> y ) finalTrans
          finalTrans                  = S.map (\(x,(Just y),z) -> (nodelabel x "", y, nodelabel z "")) $ S.filter (\(_,y,_) -> isJust y) (S.union tmpltrans (addTrans (S.toList (S.map fst nodes)) S.empty))
          tmpltrans                   = S.map (\(n1,e,n2) -> (fst n1,project e p,fst n2)) trans
          addTrans (n:xs) acc         = let eclose = epsilonReachable n
                                            newarcs =  S.map (\(_,a,q) -> (n,a,q)) $ S.filter (\(x,_,_) -> S.member x eclose) tmpltrans 
                                        in addTrans xs (S.union newarcs acc)
          addTrans [] acc             = acc
          mysucc n                    = S.map (\(_,y,z) -> (y,z)) (S.filter (\(x,_,_) -> x == n) tmpltrans)
          epsilonReachable n          = S.fromList $ traverse [n] S.empty [n]
          traverse (n:ns) visited acc = if S.member n visited  
                                        then traverse ns visited acc
                                        else let sucnodes = S.toList $ S.map (\(_,s) -> s) $ S.filter (\(l,s) -> (isNothing $ l) && (not $ S.member s visited)) $ (mysucc n)
                                             in traverse (ns++sucnodes) (S.insert n visited) (acc++sucnodes)
          traverse [] _ acc           = acc
          
firstActions :: TSb -> Configuration -> Ptp -> Set Action -> Set Action
firstActions (_,_,_,trans) n0 m goal = traverse [n0] S.empty S.empty
  where 
    traverse (n:ns) visited current
      | goal == current =  current
      | otherwise = if S.member n visited 
                    then traverse ns visited current 
                    else let pairs   = S.map (\(_,y,z) -> (y,z)) (S.filter (\(x,_,_) -> x == n) trans)
                             actions = S.map (\(Just e) -> e) $ S.filter isJust $ S.map (\(event, _) -> (project event m)) pairs
                             todo    = S.map (\(_,y) -> y) $ S.filter (\(x,_) -> isNothing x ) $ S.map (\(event, node) -> ((project event m), node)) pairs
                         in traverse (ns++(S.toList todo)) (S.insert n visited) (S.union current actions)
    traverse [] _ current = current
         
possibleActions :: System -> Ptp -> Configuration -> Set Action
possibleActions (sys,ptps) p n = S.map (\(_,y,_) -> y) $ S.filter (\(x,_,_) -> x == ((fst n)!!i)) trans
    where (_,_,_,trans) = (sys!!i)
          i             = findId p (M.assocs ptps)

ample :: TSb -> Configuration -> [Set KTrans]
ample ts conf = if allSelfLoops then [next] else L.map snd $ L.sortBy compareList events
  where next = deriv conf ts
        transList = S.toList $ S.map (\x -> (machines x, S.singleton x)) next
        events =  mygroup (length transList) transList
        fgroup (m1,_) (m2,_) = not $ S.null (S.intersection m1 m2)
        myfold xs = L.map (\ys ->  L.foldr merge (S.empty, S.empty) ys) xs
        merge (m1,e1) (m2,e2) = (S.union m1 m2, S.union e1 e2)
        --
        mygroup :: Int -> [(Set Ptp, Set KTrans)] -> [(Set Ptp, Set KTrans)] 
        mygroup i xs = let newlist = myfold (L.groupBy fgroup xs)
                       in if i > (length newlist)
                          then mygroup (length newlist) newlist
                          else xs
        compareList (_,e1) (_,e2) = compare (S.size e1) (S.size e2)
        --
        allSelfLoops = F.and $ S.map (\(x,_,z) -> x==z) next

independent:: KEvent -> KEvent -> Bool
independent (_,_,m1,m2,_,_) (_,_,m1',m2',_,_) = (m1 /= m1')  && (m1 /= m2') && (m2 /= m1')  && (m2 /= m2') 

reachableNode :: TSb -> Configuration -> [Configuration]
reachableNode ts@(confs, _, _, _) n0 = traverse (S.singleton n0) (S.singleton n0) [n0]
  where traverse border visited acc =
            if (S.null border) || (visited == confs)
            then acc -- error $ (show n0)++"\n\n"++(show  acc)
            else
                let newborder = S.fold S.union S.empty $
                                S.map (S.filter (\ y -> not $ S.member y visited) . succConfs ts) border
                in
                  traverse newborder (S.union visited newborder) (acc++(S.toList newborder))

--
-- Relations on KEvents
-- 

representativeMap :: BowTie -> Set KEvent -> Map KEvent KEvent
representativeMap bowtie events = helper (S.toList events) M.empty
    where helper [] acc     = acc
          helper (e:es) acc = case M.lookup e acc of
                               Just _  -> helper es acc
                               Nothing -> M.union (M.fromList $ L.map (\e' -> (e',e)) (equiv e)) (helper es acc)
          equiv e = S.toList $ S.fold S.union S.empty $
                    S.map (\(x,y) -> S.insert x (S.singleton y)) (S.filter (\(x,y) -> x == e || y ==e) bowtie)

getRepresentative :: Map KEvent KEvent -> KEvent -> KEvent
getRepresentative m e = case M.lookup e m of
                         Just e' -> e'
                         Nothing -> e

bowtieRel :: P -> Map Id Diamond -> Set KEvent -> BowTie
bowtieRel ptps m es = S.fromList $ (prod $ S.toList es)
    where prod evs = [(x,y) | x@(q1,q2,s,r,_,msg) <- evs, y@(q1',q2',s',r',_,msg') <- evs, 
                              (x == y) || ((msg == msg' && s == s' && r == r') &&
                                           (checkDiamond ptps m s (q1, projectUnSafe x s) (q1', projectUnSafe y s')) &&
                                           (checkDiamond ptps m r (q2, projectUnSafe x r) (q2', projectUnSafe y r'))
                                          )
                     ]

checkDiamond :: P -> Map Id Diamond -> Ptp -> (State, Action) -> (State, Action) -> Bool
checkDiamond ptps mapping sbj t t' = S.member (t,t') thisdia
  where thisdia = case M.lookup (findId sbj (M.assocs ptps)) mapping of
                   Just d -> d
                   Nothing -> S.empty

diamondMap :: System -> Map Id Diamond
diamondMap (sys,_) = helper 0 sys M.empty
  where helper i (x:xs) acc = helper (i+1) xs (M.insert i (blackdiamondRel x) acc)
        helper _ [] acc = acc

projectUnSafe :: KEvent -> Ptp -> Action
projectUnSafe (_,_,s,r,_,msg) p = if s == p
                                  then (Send,    (s,r), msg)
                                  else (Receive, (s,r), msg)
       

--
-- Building the transition system
--

-- enabled k (sys,_) (n,b)
--   PRE:  machines m in sys epsilon-free ^ m in n ^ |n| = |dom sys|
--   POST: returns the map l such that l!(i,t) is the list of transitions
--         of machine i enabled at n given the configuration
enabled :: Int -> System -> Configuration -> Map (Id,LTrans) [LTrans]
enabled k (sys, _) (n, b)
    | k == 0    =  M.fromList $ L.concat pairs
    | otherwise = helper 0 (L.concat $ L.map S.toList l)
    where rng             = range $ L.length n
          l               = L.map (\m -> (S.filter (\(_, (d, ch, msg), _) ->
                                                     (d == Send    && (length $ b!ch) < k && bs) ||
                                                     (d == Receive && (length $ b!ch) > 0 && (head $ b!ch) == msg) ||
                                                     (d == Tau)
                                                   ) (mstep m))) rng
          mstep m         = CFSM.step (sys!!m) (n!!m)
          match m t       = [t' | i <- [m+1 .. (length n-1)], t' <- (S.toList $ mstep i), dual t t']
          pairs           = L.map (\m -> L.map (\t@(q,(_,ch,msg),q') -> ((m, (q,(Tau,ch,msg),q')), (match m t))) (S.toList $ mstep m)) rng
          bs              = True -- TODO: to be used to implement 1buffer semantics
          helper _ []     = M.empty
          helper m (t:ts) = M.insert (m, t) [] (helper (m+1) ts)

-- apply conf trans = conf'
--  PRE:  trans is enabled at conf
--  POST: conf' is the update of conf after applying the transtion
apply :: P -> Configuration -> LTrans -> (KEvent, Configuration)
apply ptps c@(n, b) t@(_, (d, ch@(s,r), msg), q)
    | d == Send    = (toKEvent c ptps t, (Misc.update (findId s (M.assocs ptps)) q n, M.insert ch ((b!ch)++[msg]) b))
    | d == Receive = (toKEvent c ptps t, (Misc.update (findId r (M.assocs ptps)) q n, M.insert ch (tail $ b!ch) b))
    | d == Tau     = (toKEvent c ptps t, (Misc.update (findId r (M.assocs ptps)) q n, b))
    | otherwise    = error ((showDir d (M.empty)) ++ " not allowed")

-- step k sys conf
--  PRE:  conf is a k-bounded reachable configuration of sys
--  POST: returns the set of configurations of sys reachable in one step from
--        a configuration c
step :: Int -> System -> Configuration -> Set (KEvent, Configuration)
step k sys@(_, ptps) conf@(n,_)
    | k > 0     = S.map (\(_,t) -> apply ptps conf t) (M.keysSet ets)
    | otherwise = S.fold S.union S.empty (S.map f (M.keysSet ets))
    where ets = enabled k sys conf
          f   = \(m,t@(_, (_, (s, r), _), q)) -> let sidx = findId s (M.assocs ptps)
                                                     ridx = findId r (M.assocs ptps)
                                                 in if m==sidx
                                                    then S.fromList [(toKEvent conf ptps t, (Misc.update ridx q' (Misc.update sidx q  n), emptyBuffer ptps)) | (_, _, q') <- (ets!(m,t))]
                                                    else S.fromList [(toKEvent conf ptps t, (Misc.update ridx q  (Misc.update sidx q' n), emptyBuffer ptps)) | (_, _, q') <- (ets!(m,t))]



generate :: Int -> System -> [Configuration] -> Set Configuration -> (Set Configuration, Set KEvent, Set KTrans) -> (Set Configuration, Set KEvent, Set KTrans)
generate k sys c visited pre@(cset, eset, tset) =
  case c of 
   []      -> pre
   conf:cs -> if S.member conf visited
              then generate k sys cs visited pre
              else generate k sys ((snd nc) ++ cs) (S.insert conf visited) pre'
     where nt               = S.map (\(x,y) -> (conf, x, y)) (TS.step k sys conf)
           nc               = pol $ S.toList nt
           pre'             = (S.insert conf cset, S.union (S.fromList (fst nc)) eset, S.union nt tset)
           pol []           = ([],[])
           pol ((_,f,s):ls) = (f:(fst ll), s:(snd ll)) where ll = pol ls

-- initConf sys
--  PRE:  
--  POST: returns the initial configuration of sys
initConf :: System -> Configuration
initConf (sys,ptps) = (L.map (\(_, s, _, _) -> s) sys, M.fromList [ ((ptps!i,ptps!j),[]) | i <-rng, j <-rng, i /= j ])
    where rng = range $ (length sys)

buildTSb :: Int -> System -> TSb
buildTSb k sys
    | k > 0     = (cs, q0, es, ts)
    | otherwise = (cs, q0, (S.map (getRepresentative mapping) es), trans)
    where q0         = initConf sys
          bowtie     = bowtieRel (snd sys) (diamondMap sys) es
          trans      = S.map  (\(n, e, n') -> (n,  ((getRepresentative mapping) e), n')) ts
          mapping    = representativeMap bowtie es
          (cs,es,ts) = generate k sys [q0] S.empty (S.empty, S.empty, S.empty)

--
-- Pretty (?) Printing
--

-- nodeLabel n sep
--   PRE:  n is a node
--   POST: a string format of the node
nodelabel :: Node -> String -> String
nodelabel n sep = rmChar '\"' (helper n)
  where helper (x:y:xs) = (show x) ++ sep ++ (helper (y:xs))
        helper [x] = (show x)
        helper [] = ""      

-- eventLabel k e p flines
--   PRE:  e is an event of some participants in p
--   POST: a string format of the event
eventLabel :: Int -> KEvent -> Map String String -> String
eventLabel k (q, q', s, r, d, msg) flines
    | k < 1     =
      let interaction = s ++ flines!ggarr ++ r ++ ":" ++ (show msg) in rmChar '\"' $ 
          case flines!tslab of
            "simple" -> interaction
            _        -> "<" ++ (show q) ++ "," ++ (show q') ++ "," ++ interaction ++ ">"
    | otherwise = rmChar '\"' $ s ++ (flines!ptpsep) ++ r ++ (showDir d flines) ++ (show msg)

-- showQueue qs
--   PRE:  
--   POST: queues format for internal use
showQueue :: [String] -> String
showQueue qs = rmChar '\"' $ L.concat $ L.map tokenifymsg qs

-- displayQueue qs
--   PRE:  
--   POST: queues format for dot labels
displayQueue :: [String] -> String
displayQueue qs = "<" ++ displayMsgs qs ++ ">"
  where displayMsgs [] = ""
        displayMsgs [m] = m
        displayMsgs (m:qss) = m ++ "," ++ displayMsgs qss

-- showBuffer qs
--   PRE:  
--   POST: buffer format for dot; only non-empty buffers are shown
showBuffer :: Buffer -> String -> ([String] -> String) -> String
showBuffer b sep display = L.concat
               (L.map
                     (\((s,r),msg) -> if L.null msg then "" else sep ++ s ++ r ++ display msg)
                     (M.toAscList b))

-- showConf sys c nsep sep
--   PRE:  
--   POST: Configuration format for dot; nsep and bsep are the symbols to separate nodes and buffer elements
showConf:: Configuration -> String -> String -> ([String] -> String) -> String
showConf (n,b) sepn sepb display = (nodelabel n sepn) ++ (if sb == "" then "" else sepb) ++ sb
    where sb = showBuffer b sepb display

-- Colors the nodes of ts in TS if they satisfy a property prop in Node -> Bool
colorConf :: Int -> TSb -> (Configuration -> (Bool,String)) -> Configuration -> (Flag -> (String, String)) -> Flag -> String
colorConf _ (_,_,_,_) prop c colours f
    | cond      = ", color=" ++
                  (fst $ colours f) ++
                  ", style=filled, fillcolor=" ++
                  (snd $ colours f) ++
                  ", penwidth=2.0, fontcolor=blue, xlabel=\"" ++
                  comment ++ "\""
    | otherwise = ""
  where (cond,comment) = prop c

-- matchConfig c cpattern
--   PRE:  the words in the "node part" of cpattern have to be as many as the number of machines
--   POST: true iff c and cpattern match
matchConfig :: Int -> String -> Configuration -> Bool
matchConfig k cpattern (n,b) = (cpattern /= "") && (L.all (\(x,y) -> (y =="*" || (rmChar '\"' $ show x) == rmChar '\"' y)) (L.zip n npattern) && (k==0 || L.all (\ch -> (M.member ch b) && (b!ch /= [])) bpattern))
    where w        = words cpattern
          npattern = take (length n) w
          bpattern = pairing (w L.\\ npattern)
          pairing [] = []
          pairing ws = (ws!!0, ws!!1) : (pairing (tail $ tail ws))

-- TODO: simplify...a lot of O(n) conversion on paths can be removed if Map is not necessary
colorTrans :: TSb -> (Map String String) -> (Map Configuration [[KTrans]]) -> KTrans -> (Flag -> (String,String)) -> String
colorTrans ts flags paths tr colours
    | action     == True = ", fontcolor=" ++ a_col ++ ", color=" ++ a_col
    | onapath tr == True = ", fontcolor=" ++ p_col ++ ", color=" ++ p_col
    | otherwise                = ""
    where action     = flagAction ts tpattern tr
          tpattern   = flags ! "-tp"
          cpattern   = flags ! "-cp" -- TODO: complete
          (a_col, _) = (colours Action)
          (p_col, _) = (colours Path)
          onapath tx = (L.elem tx (L.concat $ L.concat (M.elems paths)))

-- flagDeadlock k sys ts
--   PRE:  ts in TS ^ n
--   POST: returns a function mapping each configuration c to true iff c has no outgoing transitions
flagDeadlock :: Int -> System -> TSb -> Configuration -> Bool
flagDeadlock k sys _ = \c -> ((TS.step k sys c) == S.empty && (not (L.all (\i -> S.empty == (CFSM.step (ms!!i) ((fst c)!!i))) (range $ length ms))))
    where ms = fst sys

flagAction :: TSb -> String -> KTrans -> Bool
flagAction _ pattern =
    \(_, (_, _, s, r, d, msg), _) ->
        let d' = case d of
                   Send    -> "!"
                   Receive -> "?"
                   Tau     -> ":"
                   LoopSnd -> "!"
                   LoopRcv -> "?"
            w  = words pattern
        in if (L.length w < 4)
             then False
           else (w!!0 == "*" || (show s) == w!!0) && (w!!1 == "*" || (show r) == w!!1) && (w!!2 == "*" || d' == w!!2) && (w!!3 == "*" || w!!3 == msg)


-- PRE:  .dot.cfg must be a file of lines of at least 2 words; only the first two words are considered
ts2file :: FilePath -> String -> Int -> System -> TSb -> (Map String String) -> [Cause Configuration KEvent] -> [Cause State KEvent] -> IO()
ts2file destfile sourcefile k sys ts@(confs, q0, _, trans) flags repbra _ = do
  conf <- readFile dotCFG
  let flines     = setDOT conf ---M.fromList $ L.concat $ L.map (\l -> L.map (\p -> (T.unpack $ p!!0, T.unpack $ p!!1)) [T.words l]) (T.lines $ T.pack conf)
  -- let qsep       = flines!qsep
  -- let bsep       = flines!bsep
  -- let statesep   = flines!statesep
  -- let confsep    = flines!confsep
  let --colours :: Flag -> (String, String) where the 1st string is the fg col and 2nd string is the bg 
    colours desc
      | desc == Deadlock = ( flines!deadlockcol1, flines!deadlockcol2 )
      | desc == Action   = ( flines!actioncol, "" )
      | desc == Config   = ( flines!conffgcol, flines!confbgcol )
      | desc == Path     = ( flines!pathcol, "" )
      | desc == Prop     = ( flines!propfgcol, flines!propbgcol )
      | otherwise        = ( "black", "white" )
  let diaincipit = "digraph ICTS {\ngraph [bgcolor=\"transparent\", bb=10];\n"
  let startnode  = "\"__start\" [shape = \"none\"; label=\"\";]\n{rank = same \"__start\" \"" ++ (showConf q0 (flines!qsep) (flines!bsep) showQueue) ++ "\"}\n"
  let startarrow = " [arrowhead=" ++ flines!initshape ++ "; label=\"\"; penwidth=" ++ flines!initwidth ++ "; color=" ++ flines!initcol ++ "]\n"
  let incoming   = gincoming ts
  let cpattern   = flags!"-cp"
  let tpattern   = flags!"-tp"
  let ppattern   = flags!"-p"
  let paths      = if ppattern == ""
                   then M.empty
                   else getpaths (S.toList $ (S.filter (matchConfig k (flags ! "-p")) confs))
        where getpaths []     = M.empty
              getpaths (c:cs) = gpath' ts incoming (getpaths cs) q0 [c] []
  let legend = if (M.notMember "-l" flags)
               then "subgraph legend {\n\t#rank = sink;\n\tLegend [shape=rectangle, penwidth=0, fontname=courier, fontsize=10, fillcolor=gray94, style=filled, fontcolor=coral, margin=0.1,\n\t\tlabel="
                    ++    "\"Source file             : " ++ (sourcefile)
                    ++ "\t\\lDestination file        : " ++ (destfile ++ "_ts" ++ (show k) ++ ".dot")
                    ++ "\t\\lConfiguration pattern   : " ++ (rmChar '\"' $ show cpattern)
                    ++ "\t\\lAction pattern          : " ++ (rmChar '\"' $ show tpattern)
                    ++ "\t\\lPath                    : " ++ (rmChar '\"' $ show ppattern)
                    ++ "\t\\lNumber of configurations: " ++ (show $ (S.size $ confs))
                    ++ "\t\\lNumber of transitions   : " ++ (show $ (S.size $ trans))
                    ++ "\\l\"];\n}"
               else ""
  let dianodes = startnode ++
                 (S.fold (++) "\n" (S.map (\x@(n,_) ->
                                            "\t\"" ++ (showConf x (flines!qsep) (flines!bsep) showQueue) ++ "\"\t\t\t[label=\"" ++
                                            (showConf x (flines!statesep) (flines!confsep) displayQueue) ++ "\"" ++
                                            (colorConf k ts (\y -> ((flagDeadlock k sys ts y),"")) x colours Deadlock) ++
                                            (if cpattern == "" then "" else (colorConf k ts (\y -> ((matchConfig k cpattern y),"")) x colours Config)) ++
                                            (L.concat $ L.map (\(_,c) -> colorConf k ts (\y -> (y==x,c)) x colours Prop) [(n',comment) | Rp n' _ _ comment <- repbra, n == (fst n')]) ++
                                            "];\n")
                                    confs)) ++
                 "}{\n"
  let diatrans =  "\"__start\" -> \"" ++ (showConf q0 (flines!qsep) (flines!bsep) showQueue) ++ "\"" ++ startarrow ++ (S.fold (++) "\n" $ S.map (\tr@(s,e,t) -> "\t\"" ++ (showConf s (flines!qsep) (flines!bsep) showQueue) ++ "\" -> \"" ++ (showConf t (flines!qsep) (flines!bsep) showQueue) ++ "\"\t\t\t[label=\"" ++ (eventLabel k e flines) ++ "\"" ++ (colorTrans ts flags paths tr colours) ++ "];\n") trans)
  let dest = destfile ++ "_ts" ++ (show k) ++".dot"
  writeToFile dest (diaincipit ++ legend ++ "\n\nsubgraph ts{\n" ++ dianodes ++ diatrans ++ "}}\n")
--  writeToFile ("/tmp/paths" ++ show k)
--                  (rmChar '\"' $ (L.concat $ L.map (\x -> (showConf x statesep confsep) ++ "\t" ++ (show $ paths!x) ++ "\n") (M.keys paths)))
--  writeToFile ("/tmp/incoming")
--                  (rmChar '\"' $ (L.concat $ L.map (\u -> (showConf u statesep confsep) ++ "\t\t\t" ++ (showPath $ S.toList (incoming! u)) ++ "\n")
--                                 (M.keys incoming)))
-- showPath [] = ""
-- showPath ((x,y,z):trs) = (showConf x "|" ";") ++ "\t" ++ (show y) ++ "\t" ++ (showConf z "|" ";") ++ "\n" ++ (showPath trs)

