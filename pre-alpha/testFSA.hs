import Misc
import FSA
import CFSM
import DotStuff
import SystemParser
import Data.List as L
import Data.Set as S
import System.Environment
import Data.Map.Strict as M
import System.FilePath.Posix

main :: IO ()
main = let
  a = (S.fromList ["q0","q00*qe0","qe","qe*qe0","qe0","qe0*q00","qe0*qe","qe00","qe01","qe0210","qe0220","qe022110","qe022111","qe022112","qe022120","qe022130","qe022131","qe02213210","qe02213220","qe02213221"],"q0",S.fromList [(("A","A"),Tau,"tau"),(("A","A"),Break,"break"),(("A","B"),Send,"authReq"),(("A","B"),Send,"authWithdrawal"),(("A","B"),Send,"getBalance"),(("A","C"),Send,"authFail"),(("A","C"),Send,"balance"),(("A","C"),Send,"bye"),(("A","C"),Send,"money"),(("B","A"),Receive,"allow"),(("B","A"),Receive,"balance"),(("B","A"),Receive,"denied"),(("B","A"),Receive,"deny"),(("B","A"),Receive,"granted"),(("C","A"),Receive,"*<0"),(("C","A"),Receive,">*0"),(("C","A"),Receive,"auth"),(("C","A"),Receive,"checkBalance"),(("C","A"),Receive,"quit"),(("C","A"),Receive,"withdraw")], S.fromList [("q0",(("C","A"),Receive,"auth"),"qe00"),("q00*qe0",(("A","A"),Tau,"tau"),"q0"),("qe*qe0",(("A","A"),Tau,"tau"),"qe"),("qe0",(("A","A"),Tau,"tau"),"qe0*q00"),("qe0",(("A","A"),Tau,"tau"),"qe0*qe"),("qe0",(("C","A"),Receive,"*<0"),"q00*qe0"),("qe0",(("C","A"),Receive,">*0"),"qe*qe0"),("qe0*q00",(("C","A"),Receive,"*<0"),"q0"),("qe0*qe",(("C","A"),Receive,">*0"),"qe"),("qe00",(("A","B"),Send,"authReq"),"qe01"),("qe01",(("B","A"),Receive,"denied"),"qe0210"),("qe01",(("B","A"),Receive,"granted"),"qe0220"),("qe0210",(("A","C"),Send,"authFail"),"qe0"),("qe0220",(("C","A"),Receive,"checkBalance"),"qe022110"),("qe0220",(("C","A"),Receive,"quit"),"qe022120"),("qe0220",(("C","A"),Receive,"withdraw"),"qe022130"),("qe022110",(("A","B"),Send,"getBalance"),"qe022111"),("qe022111",(("B","A"),Receive,"balance"),"qe022112"),("qe022112",(("A","C"),Send,"balance"),"qe0"),("qe022120",(("A","A"),Break,"break"),"qe0"),("qe022130",(("A","B"),Send,"authWithdrawal"),"qe022131"),("qe022131",(("B","A"),Receive,"allow"),"qe02213210"),("qe022131",(("B","A"),Receive,"deny"),"qe02213220"),("qe02213210",(("A","C"),Send,"money"),"qe0"),("qe02213220",(("A","C"),Send,"bye"),"qe02213221"),("qe02213221",(("A","A"),Break,"break"),"qe0")])
  b = (S.fromList ["q0","q00*qe0","qe","qe*qe0","qe0","qe0*q00","qe0*qe","qe00","qe01","qe0210","qe0220","qe022110","qe022111","qe022112","qe022120","qe022130","qe022131","qe02213210","qe02213220","qe02213221"],"q0",S.fromList [(("A","B"),Receive,"authReq"),(("A","B"),Receive,"authWithdrawal"),(("A","B"),Receive,"getBalance"),(("B","A"),Send,"allow"),(("B","A"),Send,"balance"),(("B","A"),Send,"denied"),(("B","A"),Send,"deny"),(("B","A"),Send,"granted"),(("B","B"),Tau,"tau"),(("B","B"),Break,"break"),(("C","B"),Receive,"*<0"),(("C","B"),Receive,">*0")], S.fromList [("q0",(("B","B"),Tau,"tau"),"qe00"),("q00*qe0",(("C","B"),Receive,"*<0"),"q0"),("qe*qe0",(("C","B"),Receive,">*0"),"qe"),("qe0",(("B","B"),Tau,"tau"),"q00*qe0"),("qe0",(("B","B"),Tau,"tau"),"qe*qe0"),("qe0",(("C","B"),Receive,"*<0"),"qe0*q00"),("qe0",(("C","B"),Receive,">*0"),"qe0*qe"),("qe0*q00",(("B","B"),Tau,"tau"),"q0"),("qe0*qe",(("B","B"),Tau,"tau"),"qe"),("qe00",(("A","B"),Receive,"authReq"),"qe01"),("qe01",(("B","A"),Send,"denied"),"qe0210"),("qe01",(("B","A"),Send,"granted"),"qe0220"),("qe0210",(("B","B"),Tau,"tau"),"qe0"),("qe0220",(("B","B"),Tau,"tau"),"qe022110"),("qe0220",(("B","B"),Tau,"tau"),"qe022120"),("qe0220",(("B","B"),Tau,"tau"),"qe022130"),("qe022110",(("A","B"),Receive,"getBalance"),"qe022111"),("qe022111",(("B","A"),Send,"balance"),"qe022112"),("qe022112",(("B","B"),Tau,"tau"),"qe0"),("qe022120",(("B","B"),Break,"break"),"qe0"),("qe022130",(("A","B"),Receive,"authWithdrawal"),"qe022131"),("qe022131",(("B","A"),Send,"allow"),"qe02213210"),("qe022131",(("B","A"),Send,"deny"),"qe02213220"),("qe02213210",(("B","B"),Tau,"tau"),"qe0"),("qe02213220",(("B","B"),Tau,"tau"),"qe02213221"),("qe02213221",(("B","B"),Break,"break"),"qe0")])
  c = (S.fromList ["q0","q00*qe0","qe","qe*qe0","qe0","qe0*q00","qe0*qe","qe00","qe01","qe0210","qe0220","qe022110","qe022111","qe022112","qe022120","qe022130","qe022131","qe02213210","qe02213220","qe02213221"],"q0",S.fromList [(("A","C"),Receive,"authFail"),(("A","C"),Receive,"balance"),(("A","C"),Receive,"bye"),(("A","C"),Receive,"money"),(("C","A"),Send,"*<0"),(("C","A"),Send,">*0"),(("C","A"),Send,"auth"),(("C","A"),Send,"checkBalance"),(("C","A"),Send,"quit"),(("C","A"),Send,"withdraw"),(("C","B"),Send,"*<0"),(("C","B"),Send,">*0"),(("C","C"),Tau,"tau"),(("C","C"),Break,"break")], S.fromList [("q0",(("C","A"),Send,"auth"),"qe00"),("q00*qe0",(("C","B"),Send,"*<0"),"q0"),("qe*qe0",(("C","B"),Send,">*0"),"qe"),("qe0",(("C","A"),Send,"*<0"),"q00*qe0"),("qe0",(("C","A"),Send,">*0"),"qe*qe0"),("qe0",(("C","B"),Send,"*<0"),"qe0*q00"),("qe0",(("C","B"),Send,">*0"),"qe0*qe"),("qe0*q00",(("C","A"),Send,"*<0"),"q0"),("qe0*qe",(("C","A"),Send,">*0"),"qe"),("qe00",(("C","C"),Tau,"tau"),"qe01"),("qe01",(("C","C"),Tau,"tau"),"qe0210"),("qe01",(("C","C"),Tau,"tau"),"qe0220"),("qe0210",(("A","C"),Receive,"authFail"),"qe0"),("qe0220",(("C","A"),Send,"checkBalance"),"qe022110"),("qe0220",(("C","A"),Send,"quit"),"qe022120"),("qe0220",(("C","A"),Send,"withdraw"),"qe022130"),("qe022110",(("C","C"),Tau,"tau"),"qe022111"),("qe022111",(("C","C"),Tau,"tau"),"qe022112"),("qe022112",(("A","C"),Receive,"balance"),"qe0"),("qe022120",(("C","C"),Break,"break"),"qe0"),("qe022130",(("C","C"),Tau,"tau"),"qe022131"),("qe022131",(("C","C"),Tau,"tau"),"qe02213210"),("qe022131",(("C","C"),Tau,"tau"),"qe02213220"),("qe02213210",(("A","C"),Receive,"money"),"qe0"),("qe02213220",(("A","C"),Receive,"bye"),"qe02213221"),("qe02213221",(("C","C"),Break,"break"),"qe0")])
  d = M.fromList [(0, "A"), (1, "B"), (2, "C")]
  s = ([a, b, c], d)
  in do progargs <- getArgs
        if L.null progargs
          then error $ usage(MIN)
          else do
              let flags =
                    getFlags MIN (take ((length progargs) - 1) progargs)
              flines <- getDotConf
              let sourcefile =
                    last progargs
              let filename =
                    takeFileName sourcefile
--              txt <- readFile sourcefile
              let (dir, _, basename, ext) =
                    setFileNames filename flags
--              let _ = lexer txt
--              let sys = parseSystem ext txt
--              let sys = [predTrxRemoval b isTau]  -- replace this with the line above in minimise.hs
              let sys = [determinise b]
              let (sys', f) = case (flags!"-D") of
                              "min" -> (L.map FSA.minimise sys, basename ++ ".min")
                              "det" -> (L.map FSA.determinise sys, basename ++ ".det")
                              "no"  -> (sys, basename ++ ".hs")
                              _     -> error ("value " ++ (flags!"-D") ++ " not appropriate for flag -D; use \"min\", \"det\", or \"no\"" )
              writeToFile (dir ++ f) (show sys')
              writeToFile (dir ++ basename ++ flags!"-D" ++ ".fsa") (system2String (sys', d))
              writeToFile (dir ++ basename ++ ".dot") (dottifySystem flines (sys, d))
              writeToFile (dir ++ "minimal_" ++ filename ++ ".txt") (show sys')
              writeToFile (dir ++ "c.txt") ("transitions\n" ++ (show $ transitionsOf $ head sys))
              putStrLn ("minimise:\tresult in " ++ dir ++ "minimal" ++ filename ++ ".txt")
              putStrLn "minimise: done"
