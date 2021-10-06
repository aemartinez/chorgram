--
-- Authors: Agustin Martinez Sune <aemartinez@dc.uba.ar>
--
-- This module implements QoS Logic for the analysis of QoS extended CFSMs
--
--

module QoSLogic where

import Data.Set as S
import Data.List as L
import Misc
import Data.Maybe
import Data.Map.Strict as M
import SyntacticGlobalChoreographies
import QoSCFSM
import PomsetSemantics
import CFSM
import TS as TS

data QL = T
    | F
    | Spec QoSSpec
    | Not QL
    | Or QL QL
    | Until QL GC QL

isSatisfiable :: QoSSystem -> QL -> Trace -> TS.Configuration -> Bool
isSatisfiable qossys prop trace conf =
    case prop of 
        T -> True
        F -> False
        Spec phi -> smtsolve (aggregation qossys trace) phi
        Not psi -> not (isSatisfiable qossys psi trace conf)
        Or psi psi' -> (isSatisfiable qossys psi trace conf) || (isSatisfiable qossys psi' trace conf)
isSatisfiable (commSys, qosattrs, qosmaps) (Until psi gchor psi') trace conf
    | isSatisfiable (commSys, qosattrs, qosmaps) psi' trace conf = True 
    | otherwise = psiHolds && nextStepHolds
        where psiHolds = isSatisfiable (commSys, qosattrs, qosmaps) psi trace conf
              nextStepHolds = S.member True (S.map filterGChorAndRecursion nextconfs)
              filterGChorAndRecursion (kevt, conf') = 
                  case kevt of (s1, s2, ptp1, ptp2, dir, msg) -> 
                                S.member extendedTrace (serializePoms $ fst (pomsetsOf gchor iter 0)) && recursiveCheck extendedTrace
                                where extendedTrace = trace ++ [((ptp1, ptp2), dir, msg)]
                                      recursiveCheck trace' = isSatisfiable (commSys, qosattrs, qosmaps) (Until psi gchor psi') trace' conf'
                                      iter = 0
              nextconfs = TS.step 0 True commSys conf 

aggregation :: QoSSystem -> Trace -> QoSSpec
aggregation qossys trace = aggregateSpecs (collectSpecs qossys trace)

aggregateSpecs :: Set QoSSpec -> QoSSpec
aggregateSpecs qosSpecSet = S.foldl aggregateTwo (S.elemAt 0 qosSpecSet) (S.deleteAt 0 qosSpecSet)
    where aggregateTwo spec spec' = M.mapWithKey (aggregateAttr spec) spec'
          aggregateAttr spec attr val = sumOfSets valInSpec val
            where valInSpec = fromJust (M.lookup attr spec)

sumOfSets :: Set Int -> Set Int -> Set Int
sumOfSets s s' = S.foldl (\set n -> S.union set (S.map (+n) s')) S.empty s

collectSpecs :: QoSSystem -> Trace -> Set QoSSpec 
collectSpecs (sys, qosattrs, qosmaps) trace = S.map (getQoSSpec qosmaps) (getStatesFromTrace sys trace)

getQoSSpec :: Map Ptp QoSMap -> (State, Ptp) -> QoSSpec
getQoSSpec qosmaps (state, ptp) = fromJust $ M.lookup state $ fromJust (M.lookup ptp qosmaps)

getStatesFromTrace :: System -> Trace -> Set (State, Ptp)
getStatesFromTrace sys@(cfsmList, p) trace = S.fromList (L.map getStateAndPtp (getKEventConfsFromTrace sys trace))
    where getStateAndPtp ( kevt , conf) = case direction kevt of Send -> (senderState kevt, sender kevt)
                                                                 Receive -> (receiverState kevt, receiver kevt)

getKEventConfsFromTrace :: System -> Trace -> [(TS.KEvent, TS.Configuration)]
getKEventConfsFromTrace sys trace = L.foldl (extendKEventConfiguration sys) firstKEventConf (init trace)
    where firstKEventConf = [nextKEventConfiguration sys (TS.initConf sys) (last trace)]

extendKEventConfiguration :: System -> [(TS.KEvent, TS.Configuration)] -> Action -> [(TS.KEvent, TS.Configuration)]
extendKEventConfiguration sys kEvtConfList act = kEvtConfList ++ [nextKEventConfiguration sys (snd $ L.last kEvtConfList) act]

nextKEventConfiguration :: System -> TS.Configuration -> Action -> (TS.KEvent, TS.Configuration)
nextKEventConfiguration sys conf act = S.elemAt 0 (S.filter matchesAction possibleSteps)
    where matchesAction (kevt, _ ) = TS.evt2interaction kevt == TS.action2interaction act
          possibleSteps = TS.step k True sys conf
          k = 0 -- k-bounded


smtsolve :: QoSSpec -> QoSSpec -> Bool
smtsolve phi phi' = True -- mock