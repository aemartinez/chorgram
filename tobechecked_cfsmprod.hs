--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--
-- Product of CFSMs
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
import FSA

parseSYS :: String -> System
parseSYS txt = (sysgrammar . lexer) txt

main :: IO ()
main =  do progargs <- getArgs
           if L.null progargs
             then error $ usage(PROD)
             else do
               let flags =
                     getFlags PROD (take ((length progargs) - 1) progargs)
               let sourcefile =
                     last progargs
               cfsmFile <- readFile sourcefile
               let ( dir, destfile, basename, ext ) =
                     setFileNames sourcefile flags
               let system =
                     case ext of
                      ".fsa" -> parseFSA (Prelude.map (\x -> words x) (lines cfsmFile))
                      ".sys" -> parseSYS cfsmFile
                      ".cms" -> parseSYS cfsmFile
                      ""     -> parseFSA (Prelude.map (\x -> words x) (lines cfsmFile))
                      _      -> error ("unknown extension " ++ ext)
               writeToFile (dir ++ ".machines") (rmChar '\"' $ show $ L.foldr (\x y -> x ++ (if y=="" then "" else " ") ++ y) "" (cfsmsIds system)) --(L.map snd (M.assocs $ snd system)))
               let bufferSize =
                     read (flags ! "-b") :: Int
               let ( ts0, tsb ) =
                     ( buildTSb 0 system, if bufferSize > 0 then buildTSb bufferSize system else ts0 )
               myPrint flags PROD ("Parsing CFSMs file..." ++ sourcefile)
               myPrint flags PROD ("dir " ++ (show $ dir))
               myPrint flags PROD ("Synchronous TS:\t(nodes " ++ (show $ (S.size $ gnodes ts0)) ++ ", transitions " ++ (show $ (S.size $ gtrans ts0)) ++ ")")
               when (bufferSize > 0) $ myPrint flags PROD ((flags ! "-b") ++ "-bounded TS:\t(nodes " ++ (show $ (S.size $ gnodes tsb)) ++ ", transitions " ++ (show $ (S.size $ gtrans tsb)) ++")")
               if flags!"-ts" == "ts"
                 then do
                    ts2file destfile sourcefile 0 system ts0 flags [] []
                    ts2file destfile sourcefile (read (flags ! "-b") :: Int) system tsb flags [] []
                 else do
                    flines  <- getDotConf
                    repbra  <- newEmptyMVar
                    braprop <- newEmptyMVar
                    proj    <- newEmptyMVar
                    prnt    <- newEmptyMVar
                    _       <- forkIO $ representabilityThread system ts0 repbra
                    _       <- forkIO $ projectionThread (dir ++ basename) system ts0 proj
                    _       <- forkIO $ printingThread system ts0 (dir ++ basename) flines prnt
                    branchingPropertyThread system ts0 braprop
                    v1 <- takeMVar repbra
                    v2 <- takeMVar braprop
                    myPrint flags PROD ("Branching representability:\t" ++ (show v1))
                    myPrint flags PROD ("Branching Property (part (ii)):\t" ++ (rmChar '\"' $ show v2))
                    _  <- takeMVar proj
                    _  <- takeMVar prnt
                    -- TODO: colour bad states
                    ts2file destfile sourcefile 0 system ts0 flags v2 v1
                    ts2file destfile sourcefile (read (flags ! "-b") :: Int) system tsb flags v2 v1
                    when (isEmpty ts0) $ myPrint flags PROD "(!)  Warning: the TS appears to be empty, synthesis will fail    (!)"
                    when (not(noSelfLoop ts0)) $ myPrint flags PROD "(!)  Warning: the TS contains a self-loop, synthesis might fail  (!)"
--
                    -- let output l =
                    --       let fname i = (mkFileName (ptps!i) dir "cfsm" ".aut") in
                    --       case l of
                    --        []   -> return ()
                    --        i:ls -> writeToFile (fname i) (cfsm2bcg (cfsms!!i) flines) >>= (\_ -> output ls)
                    -- output $ range (L.length cfsms)
--
--           putStrLn "±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± gmc ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±"
