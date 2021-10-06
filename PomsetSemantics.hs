--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module implements the pomset semantics of JLAMP 17 (but for
-- the well-formedness checking)
--

module PomsetSemantics where

import Misc
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import CFSM
import SyntacticGlobalChoreographies
import Text.XML.HXT.Parser.XmlParsec(xreadDoc)
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.TypeDefs
import DotStuff
import Data.Maybe

type Event = Int
type Lab = Map Event Action
type Pomset = (Set Event, Set (Event, Event), Lab)
type Trace = [Action]
data LabelFormat =
  Interaction |
  Communication
data DiffObj =
  Edge String String |
  Node String

serializePoms :: Set Pomset -> Set Trace
serializePoms set_of_pomsets = S.unions (S.map serializePom set_of_pomsets)

serializePom :: Pomset -> Set Trace
serializePom (events, rel, lab) = S.map events_to_actions legal_permutations
  where events_to_actions = L.map evt_to_act
        evt_to_act evt = fromJust (M.lookup evt lab) 
        legal_permutations = S.filter (checkRel rel) permutations_of_events
        permutations_of_events = S.fromList (L.permutations (S.elems events))

checkRel :: Set (Event, Event) -> [Event] -> Bool
checkRel rel evs = case evs of
                    [] -> True
                    [e] -> True
                    (e1:e2:evs) -> S.member (e1, e2) rel && checkRel rel (e2:evs)

emptySem :: Event -> (Set Pomset, Event)
-- emptySem e = (S.singleton (S.singleton e, S.empty, M.fromList [(e, (("?","?"), Tau, "?"))]), e+1)
emptySem e = (S.empty, e)

emptyPom :: Pomset
emptyPom = (S.empty, S.empty, M.empty)

eventsOf :: Pomset -> Set Event
eventsOf (events, _, _) = events

orderOf :: Pomset -> Set (Event, Event)
orderOf (_, rel, _) = rel

labelOf :: Pomset -> Lab
labelOf (_, _, lab) = lab

sprod :: Ord t => Ord t' => Set t -> Set t' -> Set (t,t')
sprod xs ys = S.fromList [(x,y) | x <- S.toList xs, y <- S.toList ys]

pomsetsOf :: GC -> Int -> Event -> (Set Pomset, Event)
pomsetsOf gg iter e =
  -- PRE: gg is well-formed; e is the 'counter' of the events
  -- POST: returns the set of pomsets [[gg]] with n-unfolds of each loop for n = |iter|
  --       (eventually) well-formedness is checked iff iter >= 0
  let unfold g n =
        if n==0
        then Emp
        else Seq (L.replicate (abs n) g)
      -- TODO: uniform unfoldind for the moment.
      --       Eventually to generate random numbers between 0 and iter.
  in
    case gg of
      Emp -> emptySem e
      Act c m ->
        let
           lab = M.fromList [(e, (c, Send, m)), (e+1, (c, Receive, m) )]
           interactionPom = (S.fromList [e, e+1], (S.singleton (e, e+1)), lab)
        in (S.singleton interactionPom, e+2)
--      LAct c m -> pomsetsOf (Act c m) iter e
      Par ggs ->
        (combine (tail pomsets) (head pomsets), e'')
        where (pomsets, e'') = L.foldl aux ([], e) ggs
              aux (gs, e') g =
                let
                  (p, e_) = pomsetsOf g iter e'
                in
                  (p : gs, e_)
              combine pps ps =
                case pps of
                  [] -> ps
                  ps':pps' ->
                    let
                      f (p, p') a =
                        S.insert (pUnion p p') a
                    in
                      combine pps' (S.foldr f S.empty (sprod ps ps'))
              pUnion = \(events, rel, lab) (events', rel', lab') ->
                (S.union events events', S.union rel rel', M.union lab lab')
      Bra ggs ->
        L.foldl aux (emptySem e) ggs
        where aux (gs, e') g =
                let
                  (p, e'') = pomsetsOf g iter e'
                in
                  (S.union p gs, e'')
      Seq ggs ->
        case ggs of
          [] -> emptySem e
          [g'] -> pomsetsOf g' iter e
          g':ggs' -> (S.map seqLeq (sprod p' p''), e'')
            where (p', e') = pomsetsOf g' iter e
                  (p'', e'') = pomsetsOf (Seq ggs') iter e'
                  -- pseq (pom@(events, rel, lab), pom'@(events', rel', lab')) =
                  --   (S.union events events',
                  --    S.union (seqrel pom pom') (S.union rel rel'),
                  --    M.union lab lab')
                  -- seqrel (events, _, lab) (events', _, lab') =
                  --   S.filter (\(e1,e2) -> case (M.lookup e1 lab, M.lookup e2 lab') of
                  --                           (Just x, Just y) -> subjectOf x == subjectOf y
                  --                           _                -> False
                  --            ) (sprod events events')
      Rep gg' _ -> pomsetsOf (unfold gg' iter) iter e

seqLeq :: (Pomset, Pomset) -> Pomset
seqLeq ((events, rel, lab), (events', rel', lab')) =
  let r =
        S.filter (\(e1,e2) ->
                    case (M.lookup e1 lab, M.lookup e2 lab') of
                      (Just x, Just y) -> subjectOf x == subjectOf y
                      _                -> False
                 ) (sprod events events')
  in
    (S.union events events',
     transitiveClosure $ S.union r (S.union rel rel'),
     M.union lab lab')

getClosure :: Set Event -> Pomset -> Set Event
getClosure evs p@(events, rel, _)=
  let p' = subpom evs p
      cont = S.difference events evs
      p'' = subpom cont p
      rel' = [(x,y) | (x,y) <- S.toList $ reflexoTransitiveClosure events rel, x /= y]
      new = S.filter (\e -> not (S.null $ getNonPred e p' rel')) (minOfPomset p'')
  in if S.null new then
       evs
     else getClosure (S.union evs new) p

-- getNonPred :: Event -> Pomset -> Set Event
-- getNonPred e p@(events, rel, _) rel' =
--   let rel' = reflexoTransitiveClosure (S.toList events) (S.toList rel)
--   in dropElems (\e' -> (e',e) € rel') (maxOfPomset p)

getNonPred :: Event -> Pomset -> [(Event, Event)] -> Set Event
getNonPred e p rel = dropElems (\e' -> (e',e) € rel) (maxOfPomset p)

mkInteractions :: Pomset -> Pomset
-- replaces matching output/input pairs of events with the
-- corresponding interaction while preserving the order
mkInteractions p@(_, rel, lab) =
  let dualpairs = S.filter getDuals rel
  in L.foldr aux p (S.toList dualpairs)
     where getDuals (e,e') = (isSend (lab!e)) && (dualAction (lab!e) == (lab!e'))
           aux (e,e') (events', rel', lab') = (S.delete e' events', replace e e' rel', M.delete e' lab')
           replace e e' r = (S.foldr (\x r' -> S.insert (change x e e') r') S.empty r)
           change (h,k) e e' = let f = \x -> if x == e' then e else x in (f h, f k)

minOfPomset :: Pomset -> Set Event
minOfPomset (events, rel, _) =
  let cod = S.map snd (S.filter (\(x,y) -> x /= y) rel)
      ismin e acc = if S.member e cod then
                      acc
                    else S.insert e acc
  in S.foldr ismin S.empty events

maxOfPomset :: Pomset -> Set Event
maxOfPomset (events, rel, _) =
  let domrel = S.map fst (S.filter (\(x,y) -> x /= y) rel)
      ismax e acc = if S.member e domrel then
                      acc
                    else S.insert e acc
  in S.foldr ismax S.empty events

subpom :: Set Event -> Pomset -> Pomset
subpom evs (_, rel, lab) = (evs, rel', lab')
-- PRE: evs included in the events of p
-- POST: returns the sub pomset made of the events in evs from p
  where rel' = S.foldr f S.empty rel
        f (h,k) res = if (S.member h evs) && (S.member k evs) then
                         S.insert (h,k) res
                      else res
        lab' = S.foldr (\e m -> M.insert e (lab!e) m) M.empty evs

components :: Pomset -> Set (Set Event)
-- computes the connected components in the order relation of the pomset
-- TODO: pretty inefficient; improve
components (events, rel, _) = S.foldr aux S.empty events
  where aux e l = S.insert (connected [] [e] S.empty) l
        connected visited tovisit acc =
          case tovisit of
            [] -> acc
            e:tovisit' ->
              if L.elem e visited then
                 connected visited tovisit' acc
              else
                let r = S.toList $ reflexoTransitiveClosure events rel
                    todo = L.map fst [(x,y) | (x,y) <- r, y == e] ++
                           L.map snd [(x,y) | (x,y) <- r, x == e]
                in connected (e:visited) (tovisit' ++ todo) (S.union acc (S.fromList todo))

pomset2gg :: Pomset -> Maybe GC
pomset2gg p@(_, _, lab) =
  let
    interactionsPomset = mkInteractions p
    comps = components interactionsPomset
    aux evs l =
      case l of
        Nothing -> Nothing
        Just l' ->
          let
            subp = subpom evs (mkInteractions p)
            closure = getClosure (S.filter (\e -> S.member e (minOfPomset subp)) evs) subp
            loop = L.foldr (\(e,e') b -> b || ((e/=e') &&  (S.member (e',e) (orderOf subp)))) False (orderOf subp)
          in -- error $ ((show subp) ++ "\n" ++ (show evs) ++ "\n" ++ (show closure) ++ "\n" ++ (show loop))
            if loop then
              Nothing
            else
              if closure == evs then
                if S.size evs > 1 then                -- A closure with more than one event
                  Nothing                             -- cannot be represented with parallel or sequential
                else                                  -- we just return the interatction
                  let act = lab!(head $ S.toList evs) -- recall that those must be output actions
                      s = subjectOf act
                      r = objectOf act
                      m = msgOf act
                  in Just ((Act (s,r) m) : l')
              else                                       -- a split is possible and we recur
                let p' = subpom closure subp
                    p'' = subpom (S.difference (eventsOf subp) closure) subp
                in
                  case (pomset2gg p', pomset2gg p'') of
                    (Nothing, _) -> Nothing
                    (_, Nothing) -> Nothing
                    (Just g1, Just g2) -> Just ((Seq [g1,g2]):l')
  in if S.null comps then
       Just Emp
     else
       -- if L.foldr (\(e,e') b -> b || (not (S.member (e',e) (orderOf interactionsPomset)))) False (orderOf interactionsPomset) then
       --   Nothing
       -- else
         let tmp = S.foldr aux (Just []) comps
         in case tmp of
              Nothing -> Nothing
              Just [gg] -> Just gg
              Just ggs -> Just (Par ggs)


-- Pomset to dot

pomset2dot :: Pomset -> String -> Map String String -> DotString
pomset2dot r name flines =
  -- transforms r in dot format giving it name 'name'
  -- and setting the size of nodes to 'nodeSize'
  let
    mkLabel i =
      (show i) ++ 
      "\t[label = \"" ++
      (printAction ((labelOf r)!i) flines) ++
      "\"]\n"
    nodes = L.map mkLabel (S.toList $ eventsOf r)
    mkEdge (i,j) = "\n\t" ++ (show i) ++ " -> " ++ (show j)
    edges = S.toList $ S.map mkEdge (orderOf r)
  in
    "digraph " ++ name ++ " {\n" ++
    -- "\t[width=" ++ (flines!"nodesize") ++ ", height=" ++ (flines!"nodesize") ++ "]\n" ++
    (L.foldr (\x y -> "\t" ++ x ++ y) "\n" (nodes ++ edges)) ++
    "}\n" 

-- GML stuff

pomset2gml :: Pomset -> String
pomset2gml (events, rel, lab) =
  -- returns the graphML representation of the pomset
  let mlpref =          "<?xml version='1.0' encoding='utf-8'?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n  <key attr.name=\"in\" attr.type=\"string\" for=\"node\" id=\"d0\" />\n  <key attr.name=\"out\" attr.type=\"string\" for=\"node\" id=\"d1\" />\n  <key attr.name=\"subject\" attr.type=\"string\" for=\"node\" id=\"d2\" />\n  <key attr.name=\"partner\" attr.type=\"string\" for=\"node\" id=\"d3\" />\n  <graph edgedefault=\"directed\">\n"
      snodetag nodeId = "    <node id=\"" ++ nodeId ++ "\">\n"
      datatag key v =   "      <data key=\"" ++ key ++ "\">" ++ v ++ "</data>\n"
      enodetag =        "    </node>\n"
      edgetab src tgt = "    <edge source=\"" ++ src ++ "\" target=\"" ++ tgt ++ "\" />\n"
      mlsuff =          "  </graph>\n</graphml>\n"
      (inkey, outkey, subjkey, othkey) = ("d0", "d1", "d2", "d3") 
      nodeGL e = (snodetag $ show e) ++ labGL e ++ enodetag
      edgeGL (e,e') = edgetab (show e) (show e')
      labGL e = case M.lookup e lab of
                  Just ((s,r), Receive, m) -> (datatag subjkey r) ++ (datatag othkey s) ++ (datatag inkey m)
                  Just ((s,r), Send,    m) -> (datatag subjkey s) ++ (datatag othkey r) ++ (datatag outkey m)
                  Just ((s,_), Tau, _)     -> (datatag subjkey s)
                  _                        -> myError POM2GC ("Unknown action: " ++ (show (M.lookup e lab)))
  in mlpref ++ (L.foldr (++) "" (S.map nodeGL events)) ++ (L.foldr (++) "" (S.map edgeGL rel)) ++ mlsuff

-- gml2pomset :: String -> Pomset
-- gml2pomset s = emptyPom
--   -- return the pomset from its gml representation
--   where 

checkTag :: QName -> String -> a -> a
checkTag tag val expr =
  if localPart tag == val then
    expr
  else error (msgFormat POM2GC "Bad " ++ val)

addKey :: [NTree XNode] -> Map String String -> Map String String
addKey xkey dict =
  case xkey of
    [NTree (XAttr attrtag) [NTree (XText k) _], _, _, NTree (XAttr idtag) [NTree (XText v) _]] ->
      checkTag attrtag "attr.name" (checkTag idtag "id" (M.insert v k dict))
    _ -> error (msgFormat POM2GC "Bad key " ++ (show xkey))

getData :: Show a => [(t, String)] -> [NTree XNode] -> a -> Map String t -> [(t, String)]
getData pairs datum msg d =
          case datum of
            [] -> pairs
            (NTree (XTag tag xkey) xval):ds ->
              checkTag tag "data" (
              let
                getPair k v =
                  case (k, v) of
                    ([NTree (XAttr tag') [NTree (XText k') _]], [NTree (XText v') _]) ->
                      checkTag tag' "key" (d!k', v')
                    _ -> error (msgFormat POM2GC "Bad key at node " ++ (show msg))
              in (getPair xkey xval):(getData pairs ds msg d)
              )
            (NTree (XText _) _):ds -> (getData pairs ds msg d)
            _ -> error (msgFormat POM2GC "Bad action at node " ++ (show msg) ++ "\t" ++ (show datum))

xgml2pomset :: String -> Pomset
xgml2pomset xml = aux (xreadDoc xml) M.empty
  where
    aux t m =
      case t of
        [] -> emptyPom
        (NTree (XPi _ _ ) _):rest -> aux rest m
        (NTree (XTag tag xtree) xtree'):rest ->
          case localPart tag of
            "key" -> aux rest (addKey xtree m)
            "graph" -> aux xtree' m
            "graphml" -> aux xtree' m
            "node" -> addNode xtree xtree' m (aux rest m)
            "edge" -> addEdge xtree (aux rest m)
            _ -> error (msgFormat POM2GC "Bad Tag " ++ (localPart tag))
        _:rest -> aux rest m
    addEdge e (events, rel, lab) =
      case e of
        [NTree (XAttr srctag) [NTree (XText s) _],
         NTree (XAttr tgttag) [NTree (XText t) _]] ->
          checkTag srctag "source" (checkTag tgttag "target" (events, S.insert ((read s)::Int, (read t)::Int) rel, lab))
        _ -> error (msgFormat POM2GC "Bad edge " ++ (show e))
    addNode n d dict (events, rel, lab) = 
      let
        nodeId = 
          case n of
            [NTree _ [NTree (XText xid) _]] -> (read xid)::Int
            _ -> error (msgFormat POM2GC "Bad node")
        action =
          let
            tmpMap = M.fromList (getData [] d nodeId dict)
          in if L.elem "in" (M.keys tmpMap) then
            ((tmpMap!"partner", tmpMap!"subject"), Receive, tmpMap!"in")
          else ((tmpMap!"subject",tmpMap!"partner"), Send, tmpMap!"out")
      in
        (S.insert nodeId events, rel, M.insert nodeId action lab)

xgml2dot :: String -> String -> Map String String -> DotString
xgml2dot name xml flines =
  -- transforms a graphml file representing a choreography (possibly
  -- violating the sgg format) in dot format
  --
  "digraph " ++ name ++ " {\n\tnode [width="    -- The string is just a dot notation with nodes and
  ++ flines!gcsizenode ++ ", height="           -- edges computed by the auxiliary function aux
  ++ flines!gcsizenode ++ "]\n"
  ++ (aux (xreadDoc xml) M.empty)
  ++ "}\n"
  where
    aux t m =
      case t of
        [] -> ""
        (NTree (XPi _ _ ) _):rest -> aux rest m
        (NTree (XTag tag xtree) xtree'):rest ->
          case localPart tag of
            "graphml" -> aux xtree' m
            "key" -> aux rest (addKey xtree m)
            "graph" -> aux xtree' m
            "node" -> addNode xtree xtree' m (aux rest m)
            "edge" -> addEdge xtree (aux rest m)
            _ -> error (msgFormat POM2GC "Bad Tag " ++ (localPart tag))
        _:rest -> aux rest m
    addEdge e dot =
      case e of
        [NTree (XAttr srctag) [NTree (XText s) []],
          NTree (XAttr tgttag) [NTree (XText t) _]] ->
          checkTag srctag "source" (
          checkTag tgttag "target" (
              dot ++ "\t" ++ s ++ " -> " ++ t ++ "\n"
              )
          )
        _ -> error (msgFormat POM2GC "Bad edge " ++ (show e))
    addNode n d dict dot =
      let
        nodeId = 
          case n of
            [NTree _ [NTree (XText xid) _]] -> xid
            _ -> error (msgFormat POM2GC "Bad node")
        tmpMap = M.fromList (getData [] d nodeId dict)
        xkeys = M.keys tmpMap
        dotline datum = nodeId ++ " " ++ 
          if L.elem "open" xkeys then
               case tmpMap!"open" of
                 "Source" -> sourceV
                 "Branch" -> branchV
                 "Fork" -> forkV
                 _ -> myError POM2GC ("Bad opening gate at node " ++ nodeId ++ "\t" ++ (show datum))
             else
               if L.elem "close" xkeys then
                 case tmpMap!"close" of
                   "Sink" -> sinkV
                   "Merge" -> mergeV
                   "Join" -> joinV
                   _ -> error (msgFormat POM2GC "Bad closing gate at node " ++ nodeId ++ "\t" ++ (show datum))
               else if L.elem "payload" xkeys then
                 "[label = \"" ++ tmpMap!"sender" ++ flines!gcarr ++ tmpMap!"receiver" ++ ":" ++ tmpMap!"payload" ++ "\", shape=rectangle, fontname=" ++ flines!nodefont ++ ", fontcolor=MidnightBlue]\n"
               else myError POM2GC ("Bad element at node " ++ nodeId ++ "\t" ++ (show datum))
      in dot ++ (dotline d)


xgmldiff2dot :: String -> String -> Map String String -> DotString
xgmldiff2dot name xml flines =
  -- transforms a graphml file representing the difference of
  -- choreographies in dot format
  "digraph " ++ name ++ " {\n\tnode [width="    -- The string is just a dot notation with nodes and
  ++ flines!gcsizenode ++ ", height="           -- edges computed by the auxiliary function aux
  ++ flines!gcsizenode ++ "]\n"
  ++ (aux (xreadDoc xml) M.empty)
  ++ "}\n"
  where
    aux t m =
      case t of
        [] -> ""
        (NTree (XPi _ _ ) _):rest -> aux rest m
        (NTree (XTag tag xtree) xtree'):rest ->
          case localPart tag of
            "graphml" -> aux xtree' m
            "key" -> aux rest (addKey xtree m)
            "graph" -> aux xtree' m
            "node" -> addNode xtree xtree' m (aux rest m)
            "edge" -> addEdge xtree xtree' (aux rest m) m
            _ -> error (msgFormat POM2GC "Bad Tag " ++ (localPart tag))
        _:rest -> aux rest m
    diffkeys = ["deleted", "changed", "kept", "added", "sender-change-to", "receiver-change-to", "payload-change-to"]
    diffRender m o l =
      let line = 
            case o of
              Edge s t -> "\t" ++ "node" ++ s ++ " -> " ++ "node" ++ t
              Node n -> n ++ " "
      in line ++ auxDiff o l
      where
        mark s ls = if L.elem s ls then " / " ++ m!s else ""
        mkdot s k = "[style=" ++ s ++ ", fillcolor=" ++ flines!k ++ ", color=" ++ flines!k ++ "]\n"
        auxDiff (Node n) ls
          | l == [] = "\n"
          | L.elem "deleted" ls = mkdot "filled" "gmldiffdel"
          | L.elem "changed" ls = mkdot "filled" "gmldiffcng"
              ++ n ++ " [label=\""
              ++ m!"sender"
              ++ mark "sender-change-to" ls
              ++ flines!gcarr
              ++ m!"receiver"
              ++ mark "receiver-change-to" ls
              ++ ":"
              ++ m!"payload"
              ++ mark "payload-change-to" ls
              ++ "\"]\n"
          | L.elem "added" ls = mkdot "filled" "gmldiffadd"
          | L.elem "kept" ls = mkdot "filled" "gmldiffkep"
          | True = myError POM2GC ("Bad key " ++ (show l))
        auxDiff (Edge _ _) ls
          | l == [] = "\n"
          | L.elem "deleted" ls = mkdot "dashed" "gmldiffdel"
          | L.elem "changed" ls = mkdot "filled" "gmldiffcng"
          | L.elem "added" ls = mkdot "filled" "gmldiffadd"
          | L.elem "kept" ls = mkdot "filled" "gmldiffkep"
          | True = myError POM2GC ("Bad key " ++ (show l))
    addEdge e d dot dict =
      let
        tmpMap = M.fromList (getData [] d () dict)
        xkeys = M.keys tmpMap
      in case e of
           [NTree (XAttr srctag) [NTree (XText s) _],
            NTree (XAttr tgttag) [NTree (XText t) _]] ->
             checkTag srctag "source" (
             checkTag tgttag "target" (
                 dot ++ (diffRender M.empty (Edge s t) (L.intersect xkeys diffkeys))
                 )
             )
           _ -> myError POM2GC ("Bad edge " ++ (show e))
    addNode n d dict dot =
      let
        nodeId = "node" ++
          case n of
            [NTree _ [NTree (XText xid) _]] -> xid
            _ -> myError POM2GC "Bad node"
        tmpMap = M.fromList (getData [] d nodeId dict)
        xkeys = M.keys tmpMap
        dotline datum = nodeId ++ " " ++
          if L.elem "open" xkeys then
               case tmpMap!"open" of
                 "Source" -> sourceV
                 "Branch" -> branchV
                 "Fork" -> forkV
                 _ -> myError POM2GC ("Bad opening gate at node " ++ nodeId ++ "\t" ++ (show datum))
             else
               if L.elem "close" xkeys then
                 case tmpMap!"close" of
                   "Sink" -> sinkV
                   "Merge" -> mergeV
                   "Join" -> joinV
                   _ -> myError POM2GC ("Bad closing gate at node " ++ nodeId ++ "\t" ++ (show datum))
               else if L.elem "payload" xkeys then
                 "[label = \"" ++ tmpMap!"sender" ++ flines!gcarr ++ tmpMap!"receiver" ++ ":" ++ tmpMap!"payload" ++ "\", shape=rectangle, fontname=" ++ flines!nodefont ++ ", fontcolor=MidnightBlue]\n"
               else myError POM2GC ("Bad element at node " ++ nodeId ++ "\t" ++ (show datum))
      in dot ++ (dotline d) ++ (diffRender tmpMap (Node nodeId) (L.intersect diffkeys xkeys))
