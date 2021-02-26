import Misc
import DotStuff
import GCParser (gcgrammar, lexer)
-- import CFSM (cfsm2String, emptyCFSM, dottifyCfsm, printCfsm)
-- import FSA (minimise,determinise)
import SyntacticGlobalChoreographies
import WellFormedness
import System.Environment
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M

mkcomm :: Bool -> String -> String
mkcomm f s = if f then "\n\n/*\n" ++ s ++ "\n*/\n\n" else s

main :: IO ()
main = do
  progargs <- getArgs
  flines <- getDotConf
  if L.null progargs
    then putStrLn $ usage WB
    else do
        let ( sourcefile, flags ) =
              getCmd WB progargs
        let ( _, _, baseName, _ ) =
              setFileNames sourcefile flags
        gctxt <- readFile sourcefile
        let ( gc, names ) =
              (gcgrammar . lexer) gctxt
        let (chk, aw) = 
              (wb gc, dependency (S.empty, S.empty) gc)
        let verbose = not(flags!"-v" == "")
        if chk == Nothing
          then putStrLn $
                 "wb" ++ (mkcomm verbose $ "\n\tcheck:\t" ++ (show (chk == Nothing)) ++ "\t\taw:\t" ++ (show aw)
          else putStrLn $
                 "non wb" ++ (mkcomm verbose $ "\n\tcheck:\t" ++ (show (chk == Nothing)) ++ "\t\taw:\t" ++ (show aw)

