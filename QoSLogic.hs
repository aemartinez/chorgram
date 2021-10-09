--
-- Authors: Agustin Martinez Sune <aemartinez@dc.uba.ar>
--
-- This module implements QoS Logic for the analysis of QoS extended CFSMs
--
--

module QoSLogic where

import SystemParser
import FSA
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

isSatisfiable :: QoSSystem -> QL -> TS.Run -> TS.Configuration -> Bool
isSatisfiable qossys@(sys, qosattrs, qosmaps) prop run conf =
    case prop of 
        T -> True
        F -> False
        Spec phi -> smtsolve (aggregation qossys run) phi
        Not psi -> not (isSatisfiable qossys psi run conf)
        Or psi psi' -> (isSatisfiable qossys psi run conf) || (isSatisfiable qossys psi' run conf)
        Until psi gchor psi' -> helper
            where helper
                      | isSatisfiable qossys psi' run conf = True 
                      | otherwise = psiHolds && nextStepHolds
                          where psiHolds = isSatisfiable qossys psi run conf
                                nextStepHolds = S.member True (S.map filterGChorAndRecursion nextconfs) 
                                -- can we use forall or exists ?
                                -- we are not implementing short circuit to discard traces not in gchor
                                -- we could do this more efficiently by returning true as soon as one extension suceeds
                                filterGChorAndRecursion (kevt@(s1, s2, ptp1, ptp2, dir, msg), conf') = 
                                        S.member (traceOf extendedRun) language && recursiveCheck extendedRun
                                        where extendedRun = run ++ [(conf, kevt, conf')]
                                              language = serializePoms $ fst (pomsetsOf gchor 0 0)
                                              recursiveCheck run' = isSatisfiable qossys prop run' conf'
                                nextconfs = TS.step 0 True sys conf
                                    -- the bound k can be computed from gchor. it is the length of the maximum trace.
                                    -- pi in L(G)
                                    -- define s(pi) = max {n : there are n interaction in pi with same sender and receiver}
                                    -- k = max {s(pi) \
                                    -- k = max {s(pi) | pi in L(G)}

traceOf :: Run -> [Action]
traceOf = L.map actionOf
    where actionOf (_, (s1, s2, ptp1, ptp2, dir, msg), _) = ((ptp1, ptp2), dir, msg)

aggregation :: QoSSystem -> Run -> QoSSpec
aggregation qossys run = aggregateSpecs (collectSpecs qossys run)

collectSpecs :: QoSSystem -> Run -> Set QoSSpec 
collectSpecs (sys, qosattrs, qosmaps) run = S.map (getQoSSpec qosmaps) (getStatesFromRun sys run)

getQoSSpec :: Map Ptp QoSMap -> (State, Ptp) -> QoSSpec
getQoSSpec qosmaps (state, ptp) = fromJust $ M.lookup state $ fromJust (M.lookup ptp qosmaps)

getStatesFromRun :: System -> Run -> Set (State, Ptp)
getStatesFromRun sys@(cfsms, ptps) run = 
    case run of [] -> (statesPtpsOf $ initConf sys) 
                ktrans@(_, _, conf'):tailRun -> S.union (statesPtpsOf conf') (getStatesFromRun sys tailRun)
        where statesPtpsOf conf@(states, buf) = M.foldrWithKey (\id ptp -> S.insert (states!!id, ptp)) S.empty ptps

aggregateSpecs :: Set QoSSpec -> QoSSpec
-- aggregateSpecs qosSpecSet
-- PRE qosSpecSet is not empty
aggregateSpecs qosSpecSet
    | (S.null qosSpecSet) = error "empty qosspecset"
    | otherwise = S.foldl aggregateTwoSpecs (S.elemAt 0 qosSpecSet) (S.deleteAt 0 qosSpecSet)

-- from here on the implementation will be dependent on the integration with smt solver

aggregateTwoSpecs :: QoSSpec -> QoSSpec -> QoSSpec
aggregateTwoSpecs spec spec' = M.mapWithKey (aggregateAttr spec) spec'
    where aggregateAttr spec attr val = (fromJust (M.lookup attr spec)) + val

smtsolve :: QoSSpec -> QoSSpec -> Bool
-- smtsolve phi phi' = True
smtsolve phi phi' = L.all id (M.elems $ M.mapWithKey (\k v -> (fromJust $ M.lookup k phi) == v) phi')