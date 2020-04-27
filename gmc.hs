--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--
-- This file contains the main program that extracts the machines from
-- a file and generates the dot file containing the CFSMs and their
-- corresponging transition system(s).
--

import SystemParser
import CFSM
import TS
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Representability
import Misc
import DotStuff
import BranchingProperty
import PetrifyBridge
import Control.Concurrent
import Control.Monad
import System.Environment
import System.Directory
import FSA

projectionThread :: FilePath -> System -> TSb -> MVar Bool -> IO ()
projectionThread filename system ts var = do
    repLanguageConcurrent filename system ts -- /!\ Concurrent version takes more memory
    putMVar var True
    
representabilityThread :: System -> TSb -> MVar [Cause State KEvent] -> IO ()
representabilityThread system ts var = do
  let repbra = repBranching system ts
  putMVar var repbra
      
-- branchingPropertyThread ::  System -> TS -> MVar Bool -> IO ()
branchingPropertyThread ::  System -> TSb -> MVar [Cause Configuration KEvent] -> IO ()
branchingPropertyThread system ts var = do
      let braprop = checkBranchingProperty system ts
      putMVar var braprop
      
printingThread :: System -> TSb -> FilePath -> Map String String -> MVar Bool -> IO ()
printingThread system ts@(confs, _, _, _) filename flines var = do
  let nodes = S.toList $ S.map fst confs
  let sigma = M.fromList $ zip nodes [[show i] | i <- [0 .. L.length nodes]]
  system2file filename ".dot" flines system
  writeToFile (filename ++ "_toPetrify") (ts2petrify (renameNodes sigma ts) (flines!qsep))
  putMVar var True

-- parseSystem :: String -> String -> System
-- parseSystem ext txt =
--   -- if 'ext' is a valid extension (.fsa, .sys, .cms), returns the parsing of txt as a system of CFSMs 
--   case ext of
--     ".fsa" -> parseFSA (Prelude.map (\x -> words x) (lines txt))
--     ".sys" -> (sysgrammar . lexer) txt
--     ".cms" -> (sysgrammar . lexer) txt
--     ""     -> parseFSA (Prelude.map (\x -> words x) (lines txt))
--     _      -> error ("unknown extension " ++ ext)

main :: IO ()
main =  do progargs <- getArgs
           if L.null progargs
             then error $ usage(GMC)
             else do
               let flags =
                     getFlags GMC (take ((length progargs) - 1) progargs)
               let sourcefile =
                     last progargs
               cfsmFile <- readFile sourcefile
               let (dir, destfile, basename, ext) =
                     setFileNames sourcefile flags
               let (sys , ptps) = parseSystem ext cfsmFile
               let sys' = case (flags!"-D") of
                            "min" -> L.map FSA.minimise sys
                            "det" -> L.map FSA.determinise sys
                            "no"  -> sys
                            _     -> error ("value " ++ (flags!"-D") ++ " not appropriate for flag -D; use \"min\", \"det\", or \"no\"" )
               let sigma (states, _, _, _) =
                     M.fromList [((S.toList states)!!i, "q" ++ show i) | i <- range (S.size states)]
               let system =
                     (if (M.member "-sn" flags) then (L.map (\cfsm -> (grenameVertex (sigma cfsm) cfsm)) sys') else sys', ptps)
               createDirectoryIfMissing True dir
               writeToFile (dir ++ ".machines") (rmChar '\"' $ show $ L.foldr (\x y -> x ++ (if y=="" then "" else " ") ++ y) "" (cfsmsIds system)) -- (L.map snd (M.assocs $ snd system)))
               let bufferSize =
                     read (flags ! "-b") :: Int
               let (ts0, tsb) =
                     (buildTSb 0 system, if bufferSize > 0 then buildTSb bufferSize system else ts0)
               myPrint flags GMC ("Parsing CFSMs file..." ++ sourcefile)
               myPrint flags GMC ("dir " ++ (show $ dir))
               myPrint flags GMC ("Synchronous TS:\t(nodes " ++ (show $ (S.size $ gnodes ts0)) ++ ", transitions " ++ (show $ (S.size $ edgesOf ts0)) ++ ")")
               when (bufferSize > 0) $ myPrint flags GMC ((flags ! "-b") ++ "-bounded TS:\t(nodes " ++ (show $ (S.size $ gnodes tsb)) ++ ", transitions " ++ (show $ (S.size $ edgesOf tsb)) ++")")
               if flags!"-ts" == "ts"
                 then do
                    ts2file destfile sourcefile 0 system ts0 flags [] []
                    ts2file destfile sourcefile (read (flags ! "-b") :: Int) system tsb flags [] []
                 else do
                    flines <- getDotConf
                    repbra <- newEmptyMVar
                    braprop <- newEmptyMVar
                    proj <- newEmptyMVar
                    prnt <- newEmptyMVar
                    createDirectoryIfMissing True dir
                    _ <- forkIO $ representabilityThread system ts0 repbra
                    _ <- forkIO $ projectionThread (dir ++ basename) system ts0 proj
                    _ <- forkIO $ printingThread system ts0 (dir ++ basename) flines prnt
                    branchingPropertyThread system ts0 braprop
                    v1 <- takeMVar repbra
                    v2 <- takeMVar braprop
                    myPrint flags GMC ("Branching representability:\t" ++ (show v1))
                    myPrint flags GMC ("Branching Property (part (ii)):\t" ++ (rmChar '\"' $ show v2))
                    _ <- takeMVar proj
                    _ <- takeMVar prnt
                    -- TODO: colour bad states
                    ts2file destfile sourcefile 0 system ts0 flags v2 v1
                    ts2file destfile sourcefile (read (flags ! "-b") :: Int) system tsb flags v2 v1
                    when (isEmpty ts0) $ myPrint flags GMC "(!)  Warning: the TS appears to be empty, synthesis will fail    (!)"
                    when (not(noSelfLoop ts0)) $ myPrint flags GMC "(!)  Warning: the TS contains a self-loop, synthesis might fail  (!)"
--
                    -- let output l =
                    --       let fname i = (mkFileName (ptps!i) dir "cfsm" ".aut") in
                    --       case l of
                    --        []   -> return ()
                    --        i:ls -> writeToFile (fname i) (cfsm2bcg (cfsms!!i) flines) >>= (\_ -> output ls)
                    -- output $ range (L.length cfsms)
--
--           putStrLn "±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± gmc ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±"
