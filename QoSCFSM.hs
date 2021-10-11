--
-- Authors: Agustin Martinez Sune <aemartinez@dc.uba.ar>
--
-- This module implements QoS extended CFSMs
--
--

module QoSCFSM where

import Data.Set as S
import Data.List as L
import Misc
import Data.Maybe
import Data.Map.Strict as M
import CFSM
import SMTLIB as SMTLIB

type QoSAttr = String
type QoSSpec = (Set QoSAttr, SMTLIB.Term)
    -- QoSSpec is one term whose only free symbols are names of QoS attributes.

-- ToDo: 
-- type QoSSpec = (Set QoSAttr, SMTLIB.Script)
--     -- Commands in the script are limited to 'declare-fun', 'declare-const', 'define-fun', 'assert'
--     -- Any undefined symbol appearing in a term will be considered a QoSAttr and has to be part of the set of QoSAttr of the QoSSystem.

type QoSMap = Map State QoSSpec
type QoSSystem  = ( System , Set QoSAttr, Map Ptp QoSMap )
-- (Map Ptp QoSMap) should be consistent with the states in Ptp
-- Each QoSSpec in QoSMap should have the same set of QoSAttr as the QoSSystem.

unionOfSpecs :: Set QoSSpec -> QoSSpec
unionOfSpecs = S.foldl acum (S.empty, SMTLIB.smtTrue)
    where acum = (\(attrSet, trm) (attrSet', trm') -> (S.union attrSet attrSet', SMTLIB.smtAND trm trm'))