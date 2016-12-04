--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--
-- This module contains function to the choreography
--

module GlobalGraph where

import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import Misc
import PetriNet
import Data.String.Utils as SU

data Vertex = ParGate (Set Vertex) (Set Vertex)
            | OrGate (Set Vertex) (Set Vertex)
            | VT Transition
            | VP Place
            | VF Flow
            | Sink (Set Vertex)
            | Source
            deriving (Ord, Eq, Show)
                     
type Arc = (Vertex, Vertex)

type GlobalGraph = (Set Vertex, Set Arc)

emptyGG :: GlobalGraph
emptyGG = (S.empty, S.empty)

nodeNumber :: GlobalGraph -> Int
nodeNumber (vs, _) = S.size vs

transNumber :: GlobalGraph -> Int
transNumber (_, as) = S.size as

sizeGG :: GlobalGraph -> (Int,Int)
sizeGG (vs,as) = (S.size vs, S.size as)

isPort :: Vertex -> Bool
isPort x = case x of
             VF _ -> True       
             _    -> False

isPlace :: Vertex -> Bool
isPlace x = case x of
              VP _ -> True
              _    -> False
                      
isTransition :: Vertex -> Bool
isTransition x = case x of
                   VT _ -> True
                   _    -> False
                           
isSilent:: Vertex -> Bool
isSilent x = case x of
               VT (Silent _) -> True
               _             -> False                           
                       
isGGNode :: Vertex -> Bool
isGGNode x = not (isPlace x || isSilent x)

isSink :: Vertex -> Bool
isSink t = case t of
             (Sink _) -> True
             _        -> False

isSource :: Vertex -> Bool
isSource t = case t of
               Source -> True
               _      -> False
                     
getSource :: GlobalGraph -> Vertex
getSource (vs, _) = case S.toList $ S.filter isSource vs of
                         (x:_) -> x
                         []     -> Source -- error $ "\n Global Graph (nodes): "++(show vs)

succG :: GlobalGraph -> Vertex -> Set Vertex
succG (_, arcs) v =  S.map (\(_,y) -> y) $ S.filter (\(x,_) -> x == v) arcs

precG :: GlobalGraph -> Vertex -> Set Vertex
precG (_, arcs) v =  S.map (\(x,_) -> x) $ S.filter (\(_,y) -> y == v) arcs

--
-- TRANSFORMATION NUMBER THREE
--
net2globalgraph :: Net -> GlobalGraph
net2globalgraph pn@(pplaces, _, events, _) = unionGG (transfPlace pplaces) (transfTrans events)
  where transfPlace ps = S.fold unionGG emptyGG $ S.map (\x -> unionGG (intoPlace pn x) (outofPlace pn x)) ps
        --
        transfTrans ts = S.fold unionGG emptyGG $ S.map (\x -> unionGG (intoTransition pn x) (outofTransition pn x)) ts


unionGG :: GlobalGraph -> GlobalGraph -> GlobalGraph
unionGG (vs1, acs1) (vs2, acs2) =  (nvs, S.union arcs addarcs) -- ((S.union vs1 vs2),(S.union acs1 acs2)) 
  where
    port1 = S.filter isPort vs1
    port2 = S.filter isPort vs2
    --
    nvs = S.difference (S.union vs1 vs2) (S.intersection port1 port2)
    arcs = S.intersection (S.union acs1 acs2) (cartProd nvs nvs)
    pairArcs1 = [ (v1,v2') | (v1,v1') <- (S.toList acs1), (v2,v2') <- (S.toList acs2), ((v1'==v2) && (isPort v2))]
    pairArcs2 = [ (v2,v1') | (v1,v1') <- (S.toList acs1), (v2,v2') <- (S.toList acs2), ((v2'==v1) && (isPort v1))]
    addarcs = S.union (S.fromList pairArcs1) (S.fromList pairArcs2) 



intoPlace :: Net -> Place -> GlobalGraph
intoPlace pn@(_, q0, _, _) p = 
  let preset = preSet pn p
  in
   if (S.size preset) == 0
   then (S.insert Source (S.singleton (VP p)) , S.singleton (Source, (VP p)))
   else if (S.size preset) > 1
        then let fws = S.map (\t -> VF $ TP(t,p)) preset
                 gate = OrGate (fws) (S.singleton (VP p))
                 vs = S.insert gate $ S.insert (VP p) $ fws
                 acs = S.insert (gate, (VP p)) (S.map (\f -> (f,gate)) fws)
             in (vs, acs)
        else -- (S.size preset) == 1
          if (not $ S.member p q0)
          then
            let t = head $ S.toList $ preset
            in (S.insert (VF $ TP(t,p)) (S.singleton (VP p)) , S.singleton (VF $ TP(t,p), VP p))
          else
            let t = head $ S.toList $ preset
            in (S.insert (VF $ TP(t,p)) (S.insert Source (S.singleton (VP p))) , 
                S.insert (Source, (VP p)) (S.singleton (VF $ TP(t,p), VP p)))
             


outofPlace :: Net -> Place -> GlobalGraph
outofPlace pn p =
  let post = postSet pn p
  in
   if (S.size post) > 1
   then let fws = S.map (\t -> VF $ PT(p,t)) post
            gate = OrGate (S.singleton (VP p)) (fws)
            vs = S.insert gate $ S.insert (VP p) $ fws
            acs = S.insert (VP p, gate) (S.map (\f -> (gate,f)) fws)
        in (vs, acs)
   else if (S.size post) == 1
        then let t = head $ S.toList $ post
             in ( S.insert (VF $ PT(p,t)) (S.singleton (VP p)) , S.singleton (VP p, VF $ PT(p,t)) )
        else (
          S.insert (Sink (S.singleton (VP p))) (S.singleton (VP p)) , 
          S.singleton (VP p, Sink (S.singleton (VP p))) 
          )
             
             
intoTransition :: Net -> Transition -> GlobalGraph
intoTransition pn t = 
  let preset = preSetTrans pn t
  in
   if (S.size preset) > 1
   then  let fws = S.map (\p -> VF $ PT(p,t)) preset
             gate = ParGate (fws) (S.singleton (VT t))
             vs = S.insert gate $ S.insert (VT t) $ fws
             acs = S.insert (gate, (VT t)) (S.map (\f -> (f,gate)) fws)
         in (vs, acs)
   else if (S.size preset) == 1
        then let p = head $ S.toList $ preset
             in (S.insert (VF $ PT(p,t)) (S.singleton (VT t)) , S.singleton (VF $ PT(p,t), VT t))
        else (S.insert Source (S.singleton (VT t)) , S.singleton (Source, (VT t)))
             
outofTransition :: Net -> Transition -> GlobalGraph
outofTransition pn t =
  let post = postSetTrans pn t
  in
   if (S.size post) > 1
   then let fws = S.map (\p -> VF $ TP(t,p)) post
            gate = ParGate (S.singleton (VT t)) (fws)
            vs = S.insert gate $ S.insert (VT t) $ fws
            acs = S.insert (VT t, gate) (S.map (\f -> (gate,f)) fws)
        in (vs, acs)
   else if (S.size post) == 1
        then let p = head $ S.toList $ post
             in ( S.insert (VF $ TP(t,p)) (S.singleton (VT t)) , S.singleton (VT t, VF $ TP(t,p)) )
        else (
          S.insert (Sink (S.singleton (VT t))) (S.singleton (VT t)) , 
          S.singleton (VT t, Sink (S.singleton (VT t))) 
          )             
             
             
             
--
-- TRANFORMATION NUMBER FOUR (CLEAN UP)
--
cleanupGG :: GlobalGraph -> GlobalGraph
cleanupGG = replaceNode
  -- let (v,a) =  replaceNode gg
  -- in (S.fold S.union S.empty $ S.map (\(x,y) -> S.insert x (S.singleton y)) a, a)
    
    
replaceNode :: GlobalGraph -> GlobalGraph
replaceNode gg@(vs, arcs) = 
  let replace = [ ( (x,p) , (p',x') ) |
                  (x,p) <- (S.toList arcs), (p',x') <- (S.toList arcs),
                  ((p==p') && not (isGGNode p) && ((S.size $ succG gg p) == 1 || (S.size $ precG gg p) == 1) )]
  in if (L.length replace) > 0
     then let ((x,p) , (_,x') ) = head replace
          in if (S.size $ succG gg p) == 1 && (S.size $ precG gg p) == 1
             then replaceNode (S.delete p vs, S.insert (x,x') (S.delete (p,x') (S.delete (x,p) arcs)) )
             else if (S.size $ precG gg p) == 1
                  then replaceNode (vs, S.insert (x,x') (S.delete (p,x) arcs) )
                  else -- (S.size $ succG p) == 1
                    replaceNode (vs, S.insert (x,x') (S.delete (x,p) arcs) )
     else gg  
--
-- PRINTING FUNCTIONS
--


ranks :: GlobalGraph -> Map Vertex Int
ranks gg = helper (getSource gg) 0 M.empty
  where helper q0 i mapping =  
          case M.lookup q0 mapping of 
            Nothing -> let list = S.toList (succG gg q0)
                           newmap = M.insert q0 i mapping
                       in
                        dovertex list (i+1) newmap
            Just _ -> mapping
        --
        dovertex (v:vs) i mapping = let newmap = helper v i mapping
                                in dovertex vs i newmap
        dovertex [] _ mapping = mapping
        
ranksToVertices :: Map Vertex Int -> Map Int (Set Vertex)
ranksToVertices  inmap = helper (M.assocs inmap) M.empty
  where helper ((v,i):xs) outmap = case M.lookup i outmap of
          Just vertices -> helper xs (M.insert i (S.insert v vertices) outmap)
          Nothing -> helper xs (M.insert i (S.singleton v) outmap)
        helper [] outmap = outmap
        

             
printVertexLabel :: Vertex -> String
printVertexLabel (VT t) = "label=\""++(SU.replace "->" " &rarr; " (printTransition t))++"\""
-- printVertexLabel _ = ""
printVertexLabel (VP p) = "label=\""++(printPlace p)++"\""
printVertexLabel f@(VF (PT(_,_))) = "label=\""++(printVertexId f)++"\""
printVertexLabel f@(VF (TP(_,_))) = "label=\""++(printVertexId f)++"\""
printVertexLabel (Sink _) = "label=\""++""++"\""
printVertexLabel (ParGate _ _) = "label=\""++"|"++"\""
printVertexLabel (OrGate _ _) = "label=\""++"+"++"\""
printVertexLabel Source = "label=\""++"\""


sizeNode :: String
sizeNode = "0.25"

printVertexGraphics :: Vertex -> String
printVertexGraphics (VT (T _)) = "shape=box"
printVertexGraphics (Sink _) = "shape=circle, width=0.2, height=0.2, fixedsize=true, peripheries=2"
printVertexGraphics (ParGate _ _) = "shape=square, fixedsize=true"
printVertexGraphics (OrGate _ _) = "shape=diamond, fixedsize=true"
printVertexGraphics Source = "shape=circle, fixedsize=true"
printVertexGraphics _ = ""


printVertexId :: Vertex -> String
printVertexId (VT t) = printTransitionId t
printVertexId (VP p) = printPlace p
printVertexId (VF (PT(p,t))) = "flow_"++(printPlace p)++"_"++(printTransitionId t)
printVertexId (VF (TP(t,p))) = "flow_"++(printTransitionId t)++"_"++(printPlace p)
printVertexId (Sink vs) = "sink_"++(S.fold (++) "" (S.map printVertexId vs))
printVertexId (Source) = "Gsource"
printVertexId (ParGate vin vout) = "par"
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vin))
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vout))
printVertexId (OrGate vin vout) = "or"
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vin))
                                   ++
                                   (S.fold (++) "" (S.map printVertexId vout))

printRanks :: GlobalGraph -> String
printRanks gg = helper listranks ""
  where listranks = (M.assocs . ranksToVertices . ranks) gg -- [(i, {v1, ... vn}]
        helper ((_,vs):xs) string = 
          let
            nodeset = S.filter (\x -> not (isSource x || isSink x)) vs
            newline = if S.null nodeset then ""
                      else "\t{rank = same; " ++ (S.fold (++) "" (S.map (\x -> (printVertexId x) ++ " ") nodeset)) ++ "}\n"
          in helper xs (string++newline)
        helper [] string = string


allSinks :: GlobalGraph -> Set Vertex
allSinks (vs, _) = S.filter isSink vs

globalGraph2String :: GlobalGraph -> String
globalGraph2String gg@(vs , arcs) = 
  let header =  "digraph GG {\nnode [width="++ (sizeNode) ++ ",height=" ++ (sizeNode) ++ "];\n\n"
      footer = "\n}\n"
      st_vertices = S.fold (++) ""
                    (S.map (\x -> "\t" ++ (printVertexId x)++" ["++(printVertexLabel x)++", "++(printVertexGraphics x)++"];\n") vs)
      st_arcs =  S.fold (++) "" (S.map (\(x,y) -> "\t" ++ (printVertexId x)++" -> "++(printVertexId y)++"; \n") arcs)
      ranksinks = let sinks = allSinks gg in
        if S.null sinks
        then ""
        else "{rank = sink; "++( S.fold (++) "" (S.map (\x -> (printVertexId x)++"; ") sinks) )++"}\n"
  in header ++
     st_vertices ++ "\n" ++
     "\t{rank = source; Gsource;}\n"++
     (printRanks gg) ++ "\n\t" ++
     ranksinks ++ "\n" ++
     st_arcs ++
     footer
     
