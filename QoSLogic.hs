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

data QL = T
        | F
        | Spec QoSSpec
        | Not QL
        | Or QL QL
        | Until QL GC QL

isSatisfiable :: QoSSystem -> QL -> State -> Trace -> Bool
isSatisfiable qossys T _ _ = True
isSatisfiable qossys F _ _ = False
isSatisfiable qossys (Spec phi) q t = smtsolve (aggregation qossys t) phi
isSatisfiable qossys (Not psi) q t = not (isSatisfiable qossys psi q t)
isSatisfiable qossys (Or psi psi') q t = (isSatisfiable qossys psi q t) || (isSatisfiable qossys psi' q t)
isSatisfiable (sys, qosattrs, qosmaps) (Until psi gc psi') q t
    | isSatisfiable qossys psi' q t = True 
    | otherwise = 


aggregation :: QoSSystem -> Trace -> QoSSpec
aggregation (sys, qosattrs, qosmaps) trace = M.fromList [] -- mock

smtsolve :: QoSSpec -> QoSSpec -> Bool
smtsolve phi phi' = True -- mock