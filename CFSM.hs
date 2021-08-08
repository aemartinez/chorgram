--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio.tuosto@gssi.it>
--

module CFSM where
import Prelude hiding (succ)
import Data.Set as S
import Data.List as L
import Misc
import Data.Foldable as F
import Data.Map.Strict as M
import DotStuff

--
-- replacing Id with some more suitable type P
-- the idea is
--    + the Ptp ps p yields the index of p in ps while
--    + Name ps p yields the name of the p-th machine in ps 
-- eventually Id should be replaced by P
--
type Ptp     = String
type Id      = Int
type P       = Map Id Ptp
type State   = String
type Cond    = String
data Dir     = Send
             | Receive
             | Tau
             | BreakLoop
             | LoopSnd
             | LoopRcv
               deriving (Eq,Ord,Show)
type Channel = ( Ptp, Ptp )
type Action  = ( Channel, Dir, Message )
type LTrans  = Edge State Action
type CFSM    = Graph State Action

--
-- Given (cfsms,ptps) :: System, ptps!i is the the identity of the machine
-- cfsms!!i
--
type System  = ( [CFSM] , P )
type Diamond = Set ( ( State, Action ), ( State, Action ) )

cfsmsIds :: System -> [Ptp]
cfsmsIds (_,ptps) = L.map snd (M.assocs ptps)

isSend :: Action -> Bool
isSend (_, d, _) = (d == Send)

isReceive :: Action -> Bool
isReceive (_, d, _) = (d == Receive)

isCommunication :: Action -> Bool
isCommunication a = (isSend a) || (isReceive a)

isTau :: Action -> Bool
isTau (_, d, _) = (d == Tau)

senderOf :: Action -> Ptp
senderOf ((s,_), _, _) = s

receiverOf :: Action -> Ptp
receiverOf ((_,r), _, _) = r

messageOf :: Action -> Ptp
messageOf (_, _, m) = m

emptyCFSM :: CFSM
emptyCFSM = ( S.empty, "", S.empty, S.empty )

stateNumber :: CFSM -> Int
stateNumber ( sts, _, _, _ ) = S.size sts

statesOf :: CFSM -> Set State
statesOf ( sts, _, _, _ ) = sts

initialOf :: CFSM -> State
initialOf ( _, q0, _, _ ) = q0

actionsOf :: CFSM -> Set Action
actionsOf ( _, _, acts, _ ) = acts

transitionsOf :: CFSM -> Set LTrans
transitionsOf ( _, _, _, trxs ) = trxs

stateNumbers :: System -> [Int]
stateNumbers ( sys, _ ) = L.map stateNumber sys

existSend :: Set Action -> Bool
existSend set = F.or $ S.map (\( _, dir, _ ) -> dir == Send || dir == LoopSnd) set

dualAction :: Action -> Action
dualAction (ch, d, msg) =
  case d of
   Send    -> ( ch, Receive, msg )
   Receive -> ( ch, Send, msg )
   LoopSnd -> ( ch, LoopRcv, msg )
   LoopRcv -> ( ch, LoopSnd, msg )
   _       -> ( ch, d, msg )

dualCFSM :: CFSM -> CFSM
dualCFSM ( states, q0, acts, trxs ) =
  ( states, q0, acts, S.map (\( q, act, q' ) -> ( q, dualAction act, q' )) trxs )

msgOf :: Action -> Message
msgOf ( _, _, msg ) = msg

subjectOf :: Action -> Ptp
subjectOf ( ( s, r ), d, _ ) =
  case d of
    Send    -> s
    LoopSnd -> s
    Receive -> r
    LoopRcv -> r
    _       -> s

objectOf :: Action -> Ptp
objectOf ( ( s, r ), d, _ ) =
  case d of
    Send    -> r
    LoopSnd -> r
    Receive -> s
    LoopRcv -> s
    _       -> r

eventOf :: LTrans -> Action
eventOf ( _, e, _ ) = e

dual :: LTrans -> LTrans -> Bool
dual tr tr' = (eventOf tr) == (dualAction (eventOf tr'))

addtrans :: LTrans -> CFSM -> CFSM
addtrans t@( q, e, q' ) ( states, q0, acts, trxs ) =
  ( (S.union (S.fromList [q,q']) states), q0, (S.insert e acts), (S.insert t trxs) )

showDir :: Dir -> Map String String -> String
showDir d flines
    | d == Send      = " " ++ flines!sndm ++ " "
    | d == Receive   = " " ++ flines!rcvm ++ " "
    | d == Tau       = " " ++ flines!tau ++ " "
    | d == BreakLoop = " " ++ flines!breakLoop ++ " "
    | d == LoopSnd   = " " ++ flines!sndm ++ " "
    | d == LoopRcv   = " " ++ flines!rcvm ++ " "
    | otherwise      = error ("Non sense: " ++ show d ++ " is not valid")

-- 
-- Basic Functions (for TS constructions)
--
step :: CFSM -> State -> Set LTrans
step ( _, _, _, trxs ) q = S.filter (\(x,_,_) -> x == q) trxs

succ :: CFSM -> State -> Action -> State
succ m q e = head $ S.toList $ S.map (\(_,_,q') -> q') (S.filter (\(_,e',_) -> e == e') (step m q))

succs :: CFSM -> State -> Set (Action, State)
succs m q =
  S.map (\( _, e, q' ) -> ( e, q' )) (step m q)

replaceState :: State -> State -> CFSM -> CFSM
-- replaceState q q' m replaces q with q' in m (substitution q |--> q')
replaceState q q' m@(states, q0, acts, trxs) =
  if q == q'
  then m
  else (S.insert q' (S.delete q states), (aux q0), acts, trxs')
    where trxs'  = S.map (\( q_, x, q_' ) -> ( aux q_, x, aux q_' )) trxs
          aux q_ = if q_ == q then q' else q_

replaceStates :: (State -> Bool) -> State -> CFSM -> CFSM
replaceStates cond q m@( states, _, _, _ ) =
  -- TODO: change this with a fold
  aux (S.toList $ S.filter cond states) m
  where aux l m' =
          case l of
           []    -> m'
           q':qs -> aux qs (replaceState q' q m')

renamePtp :: Ptp -> Ptp -> CFSM -> CFSM
renamePtp old new ( states, q0, acts, trxs ) = ( states, q0, acts', trxs' )
  where acts' = S.map ract acts
        ract  = \( ( s, r ), d, m ) -> ( (aux s, aux r), d, m )
        trxs' = S.map (\(q, act ,q') -> ( q, ract act, q' )) trxs
        aux p = if p == old then new else p

-- predTrxRemoval :: CFSM -> (Action -> Bool) -> CFSM
-- -- removes the transitions whose action satisfies the predicate lpred
-- predTrxRemoval m@(states, q0, acts, trxs) lpred = (states, q0, acts, trxs')
--   where trxs'   = S.difference (L.foldl S.union otrxs (L.map inherit pairs)) (S.filter (\(_,l,_) -> lpred l) trxs)
--         otrxs   = S.filter (\(_, l, _) -> not(lpred l)) trxs
--         pairs   = [ (q,q') | q <- S.toList states, q' <- S.toList states, not(q==q'), S.member q' (pClosure m lpred q) ]
--         inherit = \(q1,q2) -> S.map (\(_, l, q') -> (q1, l, q')) (S.intersection otrxs (goutgoing m q2))

-- flatSet :: Set State -> State
-- -- turns a set of states into a state
-- flatSet states = S.foldr ( \q q' -> (q ++ "__" ++ q') ) "" states 

-- flat :: Graph (Set State) Action -> CFSM
-- flat (states, q0, labels, trxs) =
--   (S.map flatSet states, flatSet q0, labels, S.map (\(q, l, q') -> (flatSet q, l, flatSet q')) trxs)

-- dActions :: CFSM -> State -> Dir -> Set LTrans
-- dActions m q d = (S.filter (\( _, ( _, d', _ ), _ ) -> d == d') (step m q))

-- sndActions :: CFSM -> State -> Set (Action, State)
-- sndActions m q = S.map (\( _, e , q' ) -> ( e, q' )) (dActions m q Send)

-- rcvActions :: CFSM -> State -> Set (Action, State)
-- rcvActions m q = S.map (\( _, e , q' ) -> (e, q')) (dActions m q Receive)

--
-- Diamond Computations
--
blackdiamondRel :: CFSM -> Diamond
blackdiamondRel cfsm@( _, _, _, trxs ) = Misc.equivalenceRelation q0
  where q0 = S.filter (\( x, y ) -> (keepElemt x y)) prod
        setpairs = S.map (\( x, y, _ ) -> ( x, y )) trxs
        prod = Misc.cartProd setpairs setpairs
        whitediamond = whitediamondRel cfsm
        isWhitediamond x y = S.member ( x, y ) whitediamond
        equiEvt x = equivalenceClass whitediamond x
        --
        keepPair ( s, a ) eclass =
          F.and $ S.map (\( s', a' ) -> (oneStepTrans cfsm s s') == 
                                        (oneStepTrans cfsm (succ cfsm s a) (succ cfsm s' a'))) eclass
        --
        keepElemt t1 t2 = let eclass = equiEvt t1
                          in (keepPair t1 eclass) && (keepPair t2 eclass) &&  isWhitediamond t1 t2


diamond :: CFSM -> ( State, Action ) -> ( State, Action ) -> Bool
diamond cfsm t t' = S.member ( t, t' ) (blackdiamondRel cfsm)


whitediamondRel :: CFSM -> Diamond
whitediamondRel cfsm@( _, _, _, trxs ) = Misc.equivalenceRelation q0
  where q0 = S.filter (\( x, y ) -> (diamondNonClosed cfsm x y)) prod
        setpairs = S.map (\( x, y, _ ) -> ( x, y )) trxs
        prod = Misc.cartProd setpairs setpairs
        --
--        keep (s1,a1) (s2,a2) = 
--           (oneStepTrans cfsm s1 s2) == oneStepTrans cfsm (succ cfsm s1 a1) (succ cfsm s2 a2)
          
oneStepTrans :: CFSM -> State -> State -> Set Action
oneStepTrans ( _, _, _, trxs ) s s' =
  S.map (\( _, y, _ ) -> y) (S.filter (\( x, _, z ) -> x == s && z == s') trxs)

diamondNonClosed :: CFSM -> (State, Action) -> (State, Action) -> Bool
diamondNonClosed cfsm@( _, _, _, trxs ) ( s1, a1 ) ( s2,a2 ) =
  (a1 == a2)
  &&
  (
    (s1 == s2)
    ||
    (
      not (S.null pathsrc)
      &&
      pathsrc == pathtrg
      &&
      (not $ S.member a1 pathsrc)
    )
  )
  where
    s1' = succ cfsm s1 a1
    s2' = succ cfsm s2 a2
    path s s' = S.map (\( _, y, _ ) -> y) (S.filter (\( x, _, z ) -> x == s && z == s') trxs)
    pathsrc = path s1 s2
    pathtrg = path s1' s2'

--
-- Product of CFSMs
--
stateProd :: State -> State -> State
stateProd q q' = if q' == "" then q else q ++ "*" ++ q'

-- Product of 2 CFSM
twoProd :: CFSM -> CFSM -> CFSM
twoProd m1@( _, q01, acts1, _ ) m2@( _, q02, acts2, _ ) = ( states, (stateProd q01 q02), (S.union acts1 acts2), trxs )
  where trxs  = ptrans m1 m2 [(q01,q02)] []
        states = S.fold (S.union) S.empty (S.map (\( q, _, q' ) -> S.fromList [q,q']) trxs)

ptrans :: CFSM -> CFSM -> [( State, State )] -> [( State, State )] -> Set ( State, Action, State )
ptrans _ _ [] _ = S.empty
ptrans m m' ((s,t):pairs) v =
  if ((s,t) € v)
  then ptrans m m' pairs v
  else S.union (ptrans m m' (pairs ++ newpairs) (v ++ [(s,t)])) fromst
  where st       = stateProd s t
        froms    = S.map (\( _, e, s' ) -> ( st, e, (s',t) )) (step m s)
        fromt    = S.map (\( _, e, t' ) -> ( st, e, (s,t') )) (step m' t)
        newpairs = S.toList (S.map (\( _, _, x ) -> x) (S.union froms fromt))
        fromst   = S.map (\( q, e, ( q1, q2 )) -> ( q, e, (stateProd q1 q2) )) (S.union froms fromt)


cfsmProd :: [CFSM] -> CFSM
cfsmProd [] = emptyCFSM
cfsmProd (m:ms) = if ms == []
                  then m
                  else twoProd m (cfsmProd ms)

-- cfsmUnion q0 l returns the union of the CFSMs in l with initial state q0
cfsmUnion :: State -> [CFSM] -> CFSM
cfsmUnion q0 l = (L.foldr (\(states, _, _,_) -> S.union states) (S.singleton q0) l,
                  q0,
                  L.foldr (\( _, _, acts, _ ) -> S.union acts) S.empty l,
                  L.foldr (\( _, _, _, trxs ) -> S.union trxs) S.empty l)

strToAction :: P -> Id -> [String] -> [Action]
strToAction _ _ [] = []
strToAction ptps sbj [s] = [( ((ptps!sbj), (ptps!sbj) ), Tau, "not action: " ++ s)]
strToAction ptps sbj [s,"break"] = [( (ptps!sbj, ptps!sbj ), BreakLoop, "break: " ++ s)]
strToAction ptps sbj [s,s'] = [( (ptps!sbj, ptps!sbj ), Tau, "not action: " ++ s ++ " - " ++ s')]
strToAction ptps sbj (p:d:msg:xs)
-- TODO: what about LoopSnd and LoopRcv?
  | d == "!"     = ( (ptps!sbj, ptps!(read p :: Id)), Send,      msg ):(strToAction ptps sbj xs)
  | d == "?"     = ( (ptps!(read p :: Id), ptps!sbj), Receive,   msg ):(strToAction ptps sbj xs)
  | d == "tau"   = ( (ptps!sbj, ptps!(read p :: Id)), Tau,       msg ):(strToAction ptps sbj xs)
  | d == "break" = ( (ptps!sbj, ptps!(read p :: Id)), BreakLoop, msg ):(strToAction ptps sbj xs)
  | otherwise    = ( (ptps!sbj, ptps!(read p :: Id)), Tau,       msg ++ "unknown direction: " ++ d ):(strToAction ptps sbj xs)

parseFSA :: [[String]] -> System
parseFSA text =
  --
  -- parseFSA returns a system provided that 'text' represents a few
  -- cfsms according to the following syntax:
  --     C ::= '.outputs' Str    NewLine
  --           '.state graph'    NewLine
  --           T                 NewLine
  --           '.end'            NewLine
  --     T ::= .marking Str
  --        |  Str {! + ?} Str Str NewLine T
  --
  -- where Str is a string and NewLine is the end of line token.
  -- Lines starting with '--' are interpreted as comments and
  -- ignored. 
  --
  if L.length pairs == L.length outs &&
     L.length pairs == L.length marks &&
     L.length pairs == L.length sts &&
     L.length pairs == L.length ends
  then (sys',ptps')
  else
    error ("malformed file of CFSM (some the numbers of lines starting with .outputs, .markings, .states, .end do not match)"
           ++ "\nouts\t" ++ show outs
           ++ "\nmarks\t" ++ show marks
           ++ "\nstarts\t" ++ show sts
           ++ "\nends\t" ++ show ends
          )
  where ptps' = M.fromList pairs
        pairs = [(k, names!!k) | k <- range $ L.length outs]
        outs  = L.filter (\line -> line /= [] && (head line) == ".outputs") text
        marks = L.filter (\line -> line /= [] && (head line) == ".marking") text
        sts   = L.filter (\line -> line /= [] && (head line) == ".state") text
        ends  = L.filter (\line -> line /= [] && (head line) == ".end") text
        names = getNames 0 (L.map (\line -> if tail line == [] then "" else head $ tail line) outs)
          where getNames :: Int -> [String] -> [String]
                getNames _ []     = []
                getNames i (w:os) = (if w == "" then "M"++(show i) else w) : (getNames (i+1) os)
        sys'  = parsing 0 ([( 1+l, text!!l ) | l <- range $ L.length text]) emptyCFSM []
        parsing i t m@(st, q0, ev, tr) ms =
          case t of
           ( l, x ):xs -> case x of
                           [] -> parsing i xs m ms
                           _  -> let start = head x in case start of
                                                        ".outputs" -> parsing i xs m ms
                                                        ".state"   -> parsing i xs m ms
                                                        ".marking" -> parsing i xs (st, (head $ tail x), ev, tr) ms
                                                        '-':'-':_  -> parsing i xs m ms
                                                        ".end"     -> parsing (i+1) xs emptyCFSM (ms++[m])
                                                        _          -> case tail x of
                                                                       ["break", q] -> parsing i xs (st', q0, ev', tr') ms
                                                                         where st' = S.insert start (S.insert q st)
                                                                               nt  = (start, str2act l i "break" "break" (-1), q) 
                                                                               tr' = S.insert nt tr
                                                                               ev' = S.insert (eventOf nt) ev                                                                               
                                                                       ["tau", q] -> parsing i xs (st', q0, ev', tr') ms
                                                                         where st' = S.insert start (S.insert q st)
                                                                               nt  = (start, str2act l i "tau" "tau" (-1), q) 
                                                                               tr' = S.insert nt tr
                                                                               ev' = S.insert (eventOf nt) ev                                                                               
                                                                       [p,d,msg,q] -> parsing i xs (st', q0, ev', tr') ms
                                                                         where st' = S.insert start (S.insert q st)
                                                                               nt  = (start, str2act l i d msg (read p :: Int), q) 
                                                                               tr' = S.insert nt tr
                                                                               ev' = S.insert (eventOf nt) ev
                                                                       _            -> error ("gmc: bad CFSM at line " ++ (show l) ++ ": " ++ (show x))
           []        -> ms
        str2act line sbj d msg p =
          let noLoop = (head msg /= '*') || ((head $ tail msg) /= '<') && ((head $ tail msg) /= '>')
          in case d of
               "!"     -> ( (ptps'!sbj, ptps'!p), (if noLoop then Send else LoopSnd), msg )
               "?"     -> ( (ptps'!p, ptps'!sbj), (if noLoop then Receive else LoopRcv), msg )
               "tau"   -> ( (ptps'!sbj, ptps'!sbj), Tau, msg )
               "break" -> ( (ptps'!sbj, ptps'!sbj), BreakLoop, msg )
               _   -> error ("Line " ++ show line ++ "unrecognised communication action " ++ d)


printState :: State -> String -> String
printState s sbj = sbj ++ (rmChars ['*','\"'] (show s))

replaceStateSep :: State -> String -> String
replaceStateSep s sep = case s of
                         []   -> []
                         c:s' -> (if c == '*' then sep else [c]) ++ (replaceStateSep s' sep)

prefStates :: String -> CFSM -> CFSM
prefStates pref ( states, q0, acts, trxs ) =
  ( states', pref ++ q0, acts, trxs')
  where states' = S.map (\q -> pref ++ q) states
        trxs'   = S.map (\( q,act,q') -> ( pref ++ q, act, pref ++ q' )) trxs

-- for the moment printMessage is the identity; to be changed when messages evolve
printMessage :: Message -> String
printMessage msg = msg

printAction :: Action -> Map String String -> String
printAction ( (s,r), dir, msg ) flines =
  case dir of
   LoopSnd -> show s ++ show msg
   LoopRcv -> show r ++ show msg
   _       -> rmChar '\"' $ s ++ (flines!ptpsep) ++ r ++ (showDir dir flines) ++ (printMessage msg)


prettyDotCFSM :: CFSM -> String -> Map String String -> Map Int String -> String
prettyDotCFSM ( states, q0, _, trxs ) name flines pmap =
  "subgraph " ++ name ++ " {\n\tlabel=\"" ++ name ++ "\"\n" ++ pstates ++ ptx ++ "\n}"
  where pstates =
          "\t" ++ printState (show $ S.findIndex q0 states) name ++
          "\t[style=" ++ (flines!q0style) ++
          ", color=" ++ (flines!q0col) ++ "]\n" ++
          S.foldr (++) ""
            (S.map (\x ->
                      let i = show $ S.findIndex x states
                      in "\t" ++ (printState i name) ++
                         "\t[label = \"" ++
                         (replaceStateSep (printState i name) (flines!statesep)) ++
                         "\"];\n"
                   )
              states
            )
        ptx =
          S.foldr (++) "" (
            S.map (\( s, act, t ) -> 
                  "\t" ++ (printState (show $ S.findIndex s states) name)
                  ++ " -> " ++
                  (printState (show $ S.findIndex t states) name)
                  ++
                  "\t[label = \"" ++ (prettyDotAction act flines pmap) ++ "\"];\n"
            )
            trxs
          )

prettyDotAction :: Action -> Map String String -> Map Int String -> String
prettyDotAction ( (s,r), dir, msg ) flines pmap =
  case dir of
   LoopSnd -> show s ++ show msg
   LoopRcv -> show r ++ show msg
   _       ->
     rmChar '\"' $
     (pmap!(read s :: Int)) ++
     (flines!ptpsep) ++
     (pmap!(read r :: Int)) ++
     (showDir dir flines) ++
     (printMessage msg)

printCFSM :: CFSM -> Ptp -> Map String String -> String
printCFSM ( states, q0, _, trxs ) sbj flines =
  pstates ++ ptx
  where pstates = "\t" ++ printState q0 sbj ++ "\t[style=" ++ (flines!q0style) ++ ", color=" ++ (flines!q0col) ++ "]\n" ++ S.foldr (++) "" (S.map (\x -> "\t" ++ (printState x sbj) ++ "\t[label = \"" ++ (replaceStateSep x (flines!statesep)) ++ "\"];\n") states)
        ptx     = S.foldr (++) "" (
          S.map (\( s, act, t ) -> 
                  "\t" ++ (printState s sbj)
                  ++ " -> " ++
                  (printState t sbj)
                  ++
                  "\t[label = \"" ++ (printAction act flines) ++ "\"];\n"
                )
          trxs)

dottifyCFSM :: CFSM -> Ptp -> String -> Map String String -> String
dottifyCFSM m n l flines = "digraph " ++ n ++ "{\n" ++ (printCFSM m n flines) ++ l ++ "\n}"

dottifySystem :: Map String String -> System -> String
dottifySystem flines (cfsms, ptps) =
  "digraph CFSMs {\n graph [color=white ratio=compress margin=0];\n" ++ (helper 0 cfsms) ++ "}\n\n"
  where helper i (x:xs) =
          let header  = "   subgraph cluster_" ++ (ptps!i) ++ "{\n   label = " ++ (ptps!i) ++ ";\n"
              footer  = "   }\n"
              cfsm = printCFSM x (ptps!i) flines
          in (header ++ cfsm ++ footer) ++ "\n" ++ (helper (i+1) xs)
        helper _ [] = ""

cfsm2String :: String -> CFSM -> String
cfsm2String sbj m =
  ".outputs " ++ sbj ++
  "\n.state graph\n" ++ (rmChar '\"' tx) ++
  finish
     where tx = L.concat $ L.map (\t -> (rmChar '\"' $ prt t) ++ "\n") (S.toList $ edgesOf m)
           qs = statesOf m
           finish =
             ".marking " ++
             (rmChar '\"' $ show $ S.findIndex (ginitialnode m) qs) ++
             "\n.end\n\n"
           prt t =
             (show $ S.findIndex (gsource t) qs) ++ " " ++
             (lab $ glabel t) ++ " " ++
             (show $ S.findIndex (gtarget t) qs)
           lab ( ( s, r ), dir, msg ) =
             case dir of
               Send      -> show r ++ " ! " ++ show msg
               Receive   -> show s ++ " ? " ++ show msg
               Tau       -> " tau " ++ show msg
               BreakLoop -> " break " ++ show msg
               LoopSnd   -> show r ++ " ! " ++ show msg
               LoopRcv   -> show s ++ " ? " ++ show msg

cfsm2fsm :: Map String String -> CFSM -> (String, Map State Int)
cfsm2fsm flines (states, initn, _, trans) =
  -- Transforms a cfsm into the fsm format of mcrl2; the result is a string for the fsm format and a map 
  let env = (M.fromList $ snd $  mapAccumL (\x y -> (x+1,(y,x))) 1 (S.toList states)) :: Map State Int
      liststates = intercalate "\n" $ nub $ (show $ env!initn):(L.map show $ M.elems env)
      listtrans = intercalate "\n" $
                  L.map (\(x,y,z) -> (show $ env!x)
                                     ++" "++(show $ env!z)++" \""++(printAction y flines)++"\"") (S.toList trans)
  in ("n(0)\n---\n"++liststates++"\n---\n"++listtrans++"\n" , env)

system2String :: System -> String
system2String (cfsms, ptps) =
  L.concat [ cfsm2String (ptps!i) (cfsms!!i) | i <- (range $ L.length cfsms) ]

system2file :: FilePath -> String -> Map String String -> System -> IO()
system2file file ext flines sys =
  writeToFile (file ++ "_machines" ++ ext) str
  where str = case ext of
               ".dot" -> dottifySystem flines sys
               ""     -> system2String sys
               _      -> error "Unknown extension"

