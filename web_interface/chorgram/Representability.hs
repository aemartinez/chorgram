module Representability where

import Misc
import CFSM
import TS
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import HKCBridge
import Data.Foldable as F
import Control.Concurrent
import Control.Exception
import DotStuff

--
-- This module handles Representability checks for part (ii) and projects files
-- to be checked with HKC, for part (i).
--

--
-- Language equivalence between machine and projected TS
--
repLanguageConcurrent :: FilePath -> System -> TSb -> IO()
repLanguageConcurrent file (sys,ptps) ts = 
  do x <- newEmptyMVar
     y <- newEmptyMVar
     helper sys 0 x y 
  where helper :: [CFSM] -> Id -> (MVar (Either Ptp Ptp)) -> (MVar (Either Ptp Ptp)) -> IO()
        helper (x:xs) i v1 v2 = do
          flines <- getDotConf
          let p = ptps!i
          let projected = projectTS ts p
          writeToFile (file ++ "_projection_" ++ (show i) ++ ".dot") (
            "digraph CFSM_proj_" ++ p ++ "{\n graph [color=white ratio=compress margin=0];\n" ++
            (printCfsm projected p flines) ++
            "\n}"
            )
          appendFile (file ++ "_projected") (cfsm2String p projected)
          _ <- forkIO $ 
               (writeToFile (file ++ "_machine_" ++ (show i)) (cfsm2timbuk x p)) `finally` putMVar v1 (Right p)
          _ <- forkIO $ 
               (writeToFile (file ++ "_projection_" ++ (show i)) (cfsm2timbuk (projectTS ts p) p)) `finally` putMVar v2 (Left p)
          helper xs (i+1) v1 v2
        helper [] idx v1 v2 = do waitChildren idx v1
                                 waitChildren idx v2
        --
        waitChildren idx v
          | idx > 0 = do var <- takeMVar v
                         case var of
                          Right _ -> return () -- putStrLn $ "Machine "++(show j)++" to timbuk done."
                          Left _  -> return () -- putStrLn $ "Projection "++(show j)++" to timbuk done."
                         waitChildren (idx-1) v
          | idx == 0 = return ()
        

--
-- All branchings in TS
--
repBranching :: System -> TSb -> [Cause State KEvent]
repBranching mysys@(sys,ptps) ts@(confs, _, _,_) = checkCfsms 0 [] sys
  where checkCfsms idx res (m:ms) = checkCfsms (1 + idx) (res ++ (checkCfsm idx m)) ms
        checkCfsms _ res []       = res
        checkCfsm m1 m2           = L.concat $ L.map (getCause m1 m2) (S.toList $ gnodes m2)
          where getCause p p' q = if (F.or $ S.map (\x -> S.isSubsetOf (S.map glabel (goutgoing p' q)) (readyset (ptps!p) x)) (S.filter (\(x,_) -> (x!!p) == q) confs))
                                  then []
                                  else [(Bp (ptps!p) q)]
                readyset p node = firstActions ts node p (possibleActions mysys p node)

