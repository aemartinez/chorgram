import Misc
import GCParser
import WellFormedness
import System.Environment
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = do
  progargs <- getArgs
  if L.null progargs
    then putStrLn $ usage WB
    else do
        let ( sourcefile, flags ) =
              getCmd WB progargs
        gctxt <- readFile sourcefile
        let ( gc, _ ) =
              case gcgrammar gctxt 0 0 of
                Ok x -> x
                Er err -> error err
        -- let (chk, aw) = 
        --       (wb gc, dependency (S.empty, S.empty) gc)
        let chk = 
              wb gc
        let verbose = not(flags!"-v" == "")
        case chk of
          Nothing -> putStrLn $ "wb: OK"
          Just x -> putStrLn ("wb: violation\n\t" ++ x)

