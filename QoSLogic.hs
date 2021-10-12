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
import qualified SMTLIB
import qualified SMTLIB.Solver

data QL = T
    | F
    | Spec QoSSpec
    | Not QL
    | Or QL QL
    | Until QL GC QL

isSatisfiable :: QoSSystem -> QL -> TS.Run -> TS.Configuration -> IO Bool
isSatisfiable qossys@(sys, qosattrs, qosmaps) prop run conf =
    case prop of 
         T -> return True
         F -> return False
         Spec phi -> checkEntailment (aggregation qossys run) phi
             where checkEntailment p@(attrs, term) p'@(attrs', term') = do { aux <- (SMTLIB.Solver.isSat smtScript); return $ not aux }
                     where smtScript = buildScript (S.union attrs attrs') (SMTLIB.smtAND term (SMTLIB.smtNOT term'))
         Not psi -> do 
                    aux <- (isSatisfiable qossys psi run conf)
                    return (not aux)
         Or psi psi' -> do
                        aux <- (isSatisfiable qossys psi run conf) 
                        aux' <- (isSatisfiable qossys psi' run conf)
                        return (aux || aux')
         Until psi gchor psi' -> do
                                 aux <- (isSatisfiable qossys psi' run conf)
                                 if (not (L.null run)) && aux 
                                     then return True
                                 else do
                                      aux <- isSatisfiable qossys psi run conf
                                      if aux 
                                         then S.foldl (\b kEvtConf -> do { aux <- b; aux' <- (filterGChorAndRecursion kEvtConf); return $ aux || aux'}) (return False) nextConfs 
                                      else return False
                                            where language = serializePoms $ fst (pomsetsOf gchor 0 0)
                                                  maxLength = S.findMax $ S.map L.length language
                                                  nextConfs = TS.step maxLength True sys conf
                                                  filterGChorAndRecursion (kevt, conf') = 
                                                        if L.any (L.isPrefixOf (traceOf extendedRun)) language 
                                                            then isSatisfiable qossys (Until psi gchor psi') extendedRun conf' 
                                                        else return False
                                                        where extendedRun = run ++ [(conf, kevt, conf')]

traceOf :: Run -> [Action]
traceOf = L.map actionOf
    where actionOf (_, (s1, s2, ptp1, ptp2, dir, msg), _) = ((ptp1, ptp2), dir, msg)

aggregation :: QoSSystem -> Run -> QoSSpec
aggregation (sys, qosattrs, qosmaps) run = unionOfSpecs $ S.insert (aggregationAxioms qosattrs ptpStateSet) (collectLocalSpecs qosmaps ptpStateSet)
    where ptpStateSet = (getStatesFromRun sys run)

collectLocalSpecs :: Map Ptp QoSMap -> Set (Ptp, State) -> Set QoSSpec 
collectLocalSpecs qosmaps ptpStateSet = S.map (getQoSSpec qosmaps) ptpStateSet

getQoSSpec :: Map Ptp QoSMap -> (Ptp, State) -> QoSSpec
getQoSSpec qosmaps (ptp, state) = renameAttrs ptp state qosspec
    where qosspec = fromJust $ M.lookup state $ fromJust (M.lookup ptp qosmaps)

renameAttrs :: Ptp -> State -> QoSSpec -> QoSSpec
renameAttrs ptp state spec@(attrSet, smtTerm) = (S.map f attrSet, SMTLIB.mapS f smtTerm)
    where f s = if S.member s attrSet then s ++ sufix else s
          sufix = "_" ++ ptp ++ "_" ++ state

getStatesFromRun :: System -> Run -> Set (Ptp, State)
-- getStatesFromRun s r 
-- PRE: r is a valid run of system s
-- POST: (ptp, st) is in result iff local state st of ptp appears in a configuration of run r.
getStatesFromRun sys@(cfsms, ptps) run = 
    case run of [] -> (statesPtpsOf $ initConf sys) 
                ktrans@(_, _, conf'):tailRun -> S.union (statesPtpsOf conf') (getStatesFromRun sys tailRun)
        where statesPtpsOf conf@(states, buf) = M.foldrWithKey (\id ptp -> S.insert (ptp, states!!id)) S.empty ptps

aggregationAxioms :: Set QoSAttr -> Set (Ptp, State) -> QoSSpec
aggregationAxioms attrSet ptpStateSet = S.foldl aux (attrSet, SMTLIB.smtTrue) attrSet
    where aux (atset, trm) attr = (S.union atset localAttrNames, SMTLIB.smtAND trm $ equalToCompositionTerm attr localAttrNames)
            where localAttrNames = S.map (\(ptp, state) -> attr ++ "_" ++ ptp ++ "_" ++ state) ptpStateSet

equalToCompositionTerm :: QoSAttr -> Set QoSAttr -> SMTLIB.Term
equalToCompositionTerm attr attrSet = SMTLIB.smtEq (SMTLIB.smtConstName attr) compositionTerm
    where compositionTerm = S.foldl writeSum firstAttrTrm (S.deleteAt 0 attrSet)
            where writeSum = (\trm att -> SMTLIB.smtPlus trm (SMTLIB.smtConstName att))
                  firstAttrTrm = (SMTLIB.smtConstName $ S.elemAt 0 attrSet)

buildScript :: Set QoSAttr -> SMTLIB.Term -> SMTLIB.Script
buildScript attrSet trm = SMTLIB.Script $ declareAttrs ++ [SMTLIB.Assert trm] ++ [SMTLIB.Check_sat]
    where declareAttrs = S.foldl (\cmds attr -> (declareConst attr):cmds) [] attrSet
          declareConst attr = SMTLIB.Declare_const attr (SMTLIB.Sort_identifier (SMTLIB.Identifier "Real"))