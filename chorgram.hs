--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- Command line interface to ChorGram
--

import Misc
import Data.String (lines)
import Data.List as L (foldl, null)
import System.Environment (getArgs)
import System.Process (callCommand)

main :: IO ()
main = do
  progargs <- getArgs
  if (L.null progargs)
    then do putStrLn $ usage CG
    else do
    case head progargs of
      "help" -> do
        help <- readFile "help.txt"
        putStrLn "----------------------------------- chogram info -----------------------------------"
        mapM_ (\l -> putStrLn ("\t" ++ l)) (lines help)
        putStrLn "----------------------------------- chogram info -----------------------------------"
      "minimise" -> callCommand ("project -D min " ++ (L.foldl (\x y -> x ++ " " ++ y) "" (tail progargs)))
      "determinise" -> callCommand ("project -D det " ++ (L.foldl (\x y -> x ++ " " ++ y) "" (tail progargs)))
      _ -> callCommand (L.foldl (\x y -> x ++ " " ++ y) "" progargs)
