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
    then putStrLn $ usage WB
    else do
        let ( sourcefile, flags ) =
              getCmd WS progargs
        gctxt <- readFile sourcefile
        let ( gc, _ ) =
              case gcgrammar gctxt 0 0 of
                Ok x -> x
                Er err -> error err
        let chk = 
              ws gc
        case chk of
          Nothing -> putStrLn "ws: ok"
          Just err -> putStrLn ("ws: violation" ++ err)

