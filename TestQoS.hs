--
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import qualified SystemParser
import qualified GCParser
import qualified QoSCFSM
import QoSLogic
import qualified TS
import qualified SMTLIB
import qualified SMTLIB.Solver
import System.Environment

main :: IO ()
-- main = test1
main = do 
    args <- getArgs
    case head args of
            "test1" -> test1
            "test2" -> test2
            "test3" -> test3

test1 :: IO ()
test1 = do 
    -- Read files
    let cfsmFile = "aux/experiments/tacas2021/CS.fsa"
        gchorFile = "aux/experiments/tacas2021/gchor.sgg"
        smtFileLow = "aux/experiments/tacas2021/low.smt2"
        smtFileHigh = "aux/experiments/tacas2021/high.smt2"
        phiFile = "aux/experiments/tacas2021/phi.smt2"
    cfsmTxt <- readFile cfsmFile
    smtTxtLow <- readFile smtFileLow
    smtTxtHigh <- readFile smtFileHigh
    smtPhi <- readFile phiFile
    gctxt <- readFile gchorFile
    -- Parse files
    let sys = SystemParser.parseSystem ".fsa" cfsmTxt
        smtTermLow = SMTLIB.parseAssertedTerms smtTxtLow
        smtTermHigh = SMTLIB.parseAssertedTerms smtTxtHigh
        smtTermPhi = SMTLIB.parseAssertedTerms smtPhi
        (gc, _) = case GCParser.gcgrammar gctxt 0 0 of 
            GCParser.Ok x -> x
            GCParser.Er s -> error s
    -- Build QoS comm system
        (cfsms, p) = sys
        attrSet = S.fromList ["time", "cost"]
        gammaLow = (attrSet, smtTermLow)
        gammaHigh = (attrSet, smtTermHigh)
        qosmapC = M.fromList [("0", gammaLow),("1", gammaLow),("2", gammaLow),("3", gammaLow),("4", gammaHigh),("5", gammaHigh),("6", gammaHigh),("7", gammaHigh),("8", gammaHigh)]
        qosmapS = M.fromList [("0", gammaLow),("1", gammaLow),("2", gammaLow),("3", gammaLow),("4", gammaHigh),("5", gammaHigh),("6", gammaHigh),("7", gammaHigh),("8", gammaHigh)]
        qosData = M.fromList[("C", qosmapC), ("S", qosmapS)]
        qosSystem = (sys, attrSet, qosData)
    -- Build formulas
        phi = (attrSet, smtTermPhi)
        ql = Until T gc (Spec phi)
        ql' = Until T gc T
        qlunsat = Until T gc F
    -- Call analysis
        initialConf = TS.initConf sys
    res <- isSatisfiable qosSystem ql [] initialConf
    putStrLn $ show res


test2 :: IO ()
test2 = do 
    -- Read files
    let smtFileLow = "aux/experiments/tacas2021/low.smt2"
        smtFileHigh = "aux/experiments/tacas2021/high.smt2"
    smtTxtLow <- readFile smtFileLow
    smtTxtHigh <- readFile smtFileHigh
    -- Parse files
    let smtTermLow = SMTLIB.parseAssertedTerms smtTxtLow
        smtTermHigh = SMTLIB.parseAssertedTerms smtTxtHigh
    -- Build QoSSpecs
        attrSet = S.fromList ["time", "cost"]
        gammaLow = (attrSet, smtTermLow)
        gammaHigh = (attrSet, smtTermHigh)
        (attrs', term') = renameAttrs "A" "1" gammaLow
        smtScript = buildScript (S.union attrSet attrs') (SMTLIB.smtAND term' (SMTLIB.smtNOT smtTermHigh))
    res <- SMTLIB.Solver.isSat smtScript
    putStrLn $ show res

test3 :: IO ()
test3 = do
    let attrSet = S.fromList ["time", "cost"]
        ptpStateSet = S.fromList [("A", "0"), ("A", "1"), ("B", "8")]
        aggSpec = aggregationAxioms attrSet ptpStateSet
    print aggSpec


testSMTLIB :: IO ()
testSMTLIB = do 
    -- Read files
    let cfsmFile = "aux/experiments/tacas2021/CS.fsa"
        gchorFile = "aux/experiments/tacas2021/gchor.sgg"
        smtFileLow = "aux/experiments/tacas2021/low.smt2"
        smtFileHigh = "aux/experiments/tacas2021/high.smt2"
    cfsmTxt <- readFile cfsmFile
    smtTxtLow <- readFile smtFileLow
    smtTxtHigh <- readFile smtFileHigh
    gctxt <- readFile gchorFile
    -- Parse files
    let sys = SystemParser.parseSystem ".fsa" cfsmTxt
        smtTermLow = SMTLIB.parseAssertedTerms smtTxtLow
        smtTermHigh = SMTLIB.parseAssertedTerms smtTxtHigh
        (gc, _) = case GCParser.gcgrammar gctxt 0 0 of GCParser.Ok x -> x
    -- Build QoS comm system
        (cfsms, p) = sys
        attrSet = S.fromList ["time", "cost"]
        gammaLow = (attrSet, smtTermLow)
        gammaHigh = (attrSet, smtTermHigh)
        qosmapC = M.fromList [("0", gammaLow),("1", gammaLow),("2", gammaLow),("3", gammaLow),("4", gammaHigh),("5", gammaHigh),("6", gammaHigh),("7", gammaHigh),("8", gammaHigh)]
        qosmapS = M.fromList [("0", gammaLow),("1", gammaLow),("2", gammaLow),("3", gammaLow),("4", gammaHigh),("5", gammaHigh),("6", gammaHigh),("7", gammaHigh),("8", gammaHigh)]
        qosData = M.fromList[("C", qosmapC), ("S", qosmapS)]
        qosSystem = (sys, attrSet, qosData)
    -- Build formulas
        phi = gammaLow
        ql = Until T gc (Spec phi)
        ql' = Until T gc T
        qlunsat = Until T gc F
    -- Call analysis
        initialConf = TS.initConf sys
    res <- isSatisfiable qosSystem ql' [] initialConf
    putStrLn $ show res