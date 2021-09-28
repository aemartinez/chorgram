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

type QoSAttr = String
type QoSSpec = Map QoSAttr (Set Int) -- ToDo: define QoSSpec as theory over real numbers

type QoSMap = Map State QoSSpec
type QoSSystem  = ( System , Set QoSAttr, Map Id QoSMap )