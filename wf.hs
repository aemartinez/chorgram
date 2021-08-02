import Misc
import GCParser
import WellFormedness
import System.Environment
import Data.List as L
import DotStuff

main :: IO ()
main = do
  progargs <- getArgs
  flines <- getDotConf
  if L.null progargs
    then putStrLn $ usage WF
    else do
        let ( sourcefile, flags ) =
              getCmd WF progargs
        gctxt <- readFile sourcefile
        let ( gc, _ ) =
              case gcgrammar gctxt 0 0 of
                Ok x -> x
                Er err -> error err
        let chk = 
              wf gc
        case chk of
          Nothing -> putStrLn "wf: ok"
          Just err -> putStrLn ("wf: violation\n" ++ (show $ fst err) ++ "\n" ++ (show $ snd err))

