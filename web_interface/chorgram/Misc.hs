--
-- Authors: Julien Lange <j.lange@ic.ac.uk> and
--          Emilio Tuosto <emilio@le.ac.uk>
--
-- This module contains some utility functions

module Misc where

import Data.List as L
import Data.Set as S
import Data.Map.Strict as M
import System.FilePath.Posix

type Message             = String
type Atrans vertex label = (vertex, label, vertex)
type Agraph vertex label = (Set vertex, vertex, Set label, Set(Atrans vertex label))

data Command = GMC | GG | SGG | SYS
data Flag    = Deadlock | Action | Config | Path | Prop deriving (Eq)

-- Some useful functions

list2map :: [a] -> Map Int a
list2map l = M.fromList [(p,l!!p) | p <- [0 .. (L.length l) - 1 ] ]

intersect :: (Ord a) => Set a -> Set a -> Bool
intersect x y = not(S.null (S.intersection x y))

findId :: (Eq a, Show a, Show k) => a -> [(k, a)] -> k
findId e m = case m of
              []        -> error ("Element " ++ show e ++ " not in the map " ++ show m)
              (i,e'):ms -> if e==e' then i else (findId e ms)

isSublist :: (Eq a) => [a] -> [a] -> Bool
isSublist l1 l2 =
  case l1 of
    []   -> True
    e:ls -> e € l2 && (isSublist ls l2)

range :: Int -> [Int]
range i
  | i <= 0    = [i+1 .. 0]
  | otherwise = [0 .. i-1]

-- For parsing: strings are made of the following characters
--
--   0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz/$#&~()\"
--
-- and must start with a letter when specifying the identity of a
-- machine (non-terminal Ptp) or of system.
isAlpha :: Char -> Bool
isAlpha c = c € ([x | x <- ['0'.. 'z'] ++ ['/', '$', '#', '&', '~', '\"'],
		      not (x € ['@', '.', ',', ';', ':', '(', ')', '[', ']', '{', '}', '|', '+', '*', '!', '?', '-', '%', '§'])
                 ])

-- Names of participants have to begin with a letter
isPtp :: String -> Bool
isPtp s = (L.null s) || ( ((head s) € (['a'.. 'z'] ++ ['A'.. 'Z'])) && (L.all isAlpha (tail s)) )

-- PRE:  
-- POST: -1 if e not in l, otherwise first position of e in the list plus the offset o
idxOffset :: (Num a, Eq b) => b -> [b] -> a -> a
idxOffset e l o
  | l == []   = -1
  | otherwise = if e == (head l) then o else (idxOffset e (tail l) (o+1))

rmChar :: Char -> String -> String
rmChar c (x:xs) = if x == c then rmChar c xs else x : (rmChar c xs)
rmChar _ [] = "" 

replaceChar :: Char -> Char -> String -> String
replaceChar c c' s = case s of
                      [] -> s
                      c'':s' -> (if c''==c then c' else c''):(replaceChar c c' s')

rmChars :: String -> String -> String
rmChars cs (x:xs) = if x € cs then rmChars cs xs else x : (rmChars cs xs)
rmChars _ [] = "" 

rmveStars :: String -> String            
rmveStars = \s -> rmChar '*' s 

rmQuotes :: String -> String            
rmQuotes = \s -> rmChar '\"' s

update :: Int -> a -> [a] -> [a]
update i v l
     | i == 0    = v:(tail l)
     | otherwise = (head l) : Misc.update (i-1) v (tail l)

(€) :: (Eq a) => a -> [a] -> Bool
x € y = L.elem x y

transitiveClosureParam :: (Eq a) => (a -> a -> Bool) -> [(a, a)] -> [(a, a)]
transitiveClosureParam f closure 
  | closure == closureUntilNow = closure
  | otherwise                  = transitiveClosureParam f closureUntilNow
  where closureUntilNow = 
          L.nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, ((f b b') || (f b' b))]
          
minR :: (Eq a) => [(a,a)] -> [(a,a)]
minR as = [ (n,m) | (n,m) <- as, L.all (\(_,t) -> n/=t) as ]

maxR :: (Eq a) => [(a,a)] -> [(a,a)]
maxR as = [ (n,m) | (n,m) <- as, L.all (\(t,_) -> m/=t) as ]


-- Assuming that (x,x) in ys for all x
equivalenceRelation :: (Eq a) => [(a,a)] -> [(a,a)]
equivalenceRelation ys = transitiveClosure (L.nub $ addPairs ys)
    where addPairs ((x,y):xs) =  if x/=y
                                 then (x,y):((y,x):(addPairs xs))
                                 else (y,x):(addPairs xs)
          addPairs [] = []
        

equivalenceClass :: (Eq a, Ord a) => Set (a,a) ->  a -> Set a
equivalenceClass rel z = S.fold (S.union) S.empty $ 
                         S.map (\(x,y) -> S.insert x (S.singleton y)) (S.filter (\(x,y) -> x == z || y ==z) rel)
          
-- http://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
transitiveClosure :: (Eq a) =>  [(a, a)] -> [(a, a)]
transitiveClosure closure
    | closure == closureUntilNow = closure
    | otherwise                  = transitiveClosure closureUntilNow
    where closureUntilNow = L.nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, b == b']
        
cartProd :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
cartProd sa sb = S.fromList $ [(x,y) | x <- (S.toList sa), y <- (S.toList sb)]

justFilter :: (Ord a) => Set (Maybe a) -> Set a
justFilter set = helper (S.toList set)
    where helper (x:xs) = case x of 
                            Just y -> S.insert y (helper xs)
                            Nothing -> helper xs
          helper [] = S.empty

pairsof :: [a] -> [(a,a)]
pairsof xs = zip (everyOtherEl xs) (everyOtherEl (tail xs))
    where everyOtherEl xs' = (head xs') : (everyOtherEl (tail xs'))

--
-- Some IO utils
--

dirpath :: String
dirpath = "experiments/results/"

nop :: IO ()
nop = sequence_ []

verbose :: Map String String -> String -> String -> String -> IO()
verbose m f v s = do if m ! f == v then (putStrLn $ s) else nop

setFileNames :: String -> Map String String -> (String, String, String, String)
setFileNames f flags = (dir, dir ++ baseFile, baseFile , takeExtension f)
  where baseFile = takeBaseName f
        dir      = let d = flags!"-d" in
                    case d of
                     "" -> dirpath
                     _  -> (if (d!!((length d) - 1) == pathSeparator) then d else d ++ [pathSeparator]) ++ baseFile ++ [pathSeparator]

rmExtension :: String -> String -> String
rmExtension ext s = if (drop i s == ext) then fst $ splitAt i s else s
  where i = L.length s - L.length ext
  
writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content

-- Message on how to use a command
usage :: Command -> String
usage cmd = "Usage: " ++ msg
  where msg = case cmd of
               GMC -> "gmc [-b | --bound number] [-m | --multiplicity number] [-d | --dir dirpath] [-fs | --fontsize fontsize] [-ts] [-cp cpattern] [-tp tpattern] [-v] [-l] filename \n   defaults: \t bound = 0 \n\t\t mutiplicity = 0 \n\t\t dirpath = " ++ dirpath ++ "\n\t\t fontsize = 8 \n\t\t cpattern = \"\" \n\t\t tpattern = \"- - - -\"\n"
               GG  -> "BuildGlobal [-d | --dir dirpath] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               SGG -> "sgg [-d dirpath] [-l] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               SYS -> "systemparser [-d dirpath] [-l] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"

msgFormat :: Command -> String -> String
msgFormat cmd msg =
  let pre = case cmd of
        GMC -> "gmc:\t"
        GG  -> "gg:\t"
        SGG -> "sgg:\t"
        SYS -> "systemparser:\t"
  in pre ++ msg


-- The default argument of each command
defaultFlags :: Command -> Map String String
defaultFlags cmd = case cmd of
                     GMC -> M.fromList [("-d",dirpath), ("-v",""), ("-m","0"), ("-ts",""), ("-b","0"), ("-cp", ""), ("-tp", "- - - -"), ("-p", "")]
                     GG  -> M.fromList [("-d",dirpath), ("-v","")]
                     SGG -> M.fromList [("-d",dirpath)]
                     SYS -> M.fromList [("-d",dirpath)]

getFlags :: Command -> [String] -> Map String String
getFlags cmd args = case cmd of
                      GMC -> case args of
                              []                -> defaultFlags(cmd)
                              "-v":xs           -> M.insert "-v" "v"   (getFlags cmd xs)
                              "-l":xs           -> M.insert "-l" "no"  (getFlags cmd xs)
                              "-ts":xs          -> M.insert "-ts" "ts" (getFlags cmd xs)
                              "-b":y:xs         -> M.insert "-b"  y    (getFlags cmd xs)
                              "-m":y:xs         -> M.insert "-m"  y    (getFlags cmd xs)
                              "--muliply":y:xs  -> M.insert "-m"  y    (getFlags cmd xs)
                              "-d":y:xs         -> M.insert "-d"  y    (getFlags cmd xs)
                              "--dir":y:xs      -> M.insert "-d"  y    (getFlags cmd xs)
                              "--fontsize":y:xs -> M.insert "-fs" y    (getFlags cmd xs)
                              "-cp":y:xs        -> M.insert "-cp" y    (getFlags cmd xs)
                              "-tp":y:xs        -> M.insert "-tp" y    (getFlags cmd xs)
                              "-p":y:xs         -> M.insert "-p"  y    (getFlags cmd xs)
                              _                 -> error $ usage(cmd)
                      GG  ->  case args of
                                []     -> defaultFlags(cmd)
                                [x]    -> case x of
                                           "-v"  -> M.insert "-v" "v" (defaultFlags(cmd))
                                           _     -> error $ usage(cmd)
                                x:y:xs -> case x of
                                           "-d" -> M.insert "-d"  y (getFlags cmd xs)
                                           "-v" -> M.insert "-v" "v" (getFlags cmd (y:xs))
                                           _    -> error $ usage(cmd)
                      SGG -> case args of
                              []        -> defaultFlags(cmd)
                              "-d":y:xs -> M.insert "-d"  y    (getFlags cmd xs)
                              _         -> error $ usage(SGG)
                      SYS -> case args of
                              []        -> defaultFlags(cmd)
                              "-d":y:xs -> M.insert "-d"  y    (getFlags cmd xs)
                              _         -> error $ usage(cmd)

--
-- Some utilities on graphs
--

-- Projections of Agraph and Atrans components

gnodes :: Agraph vertex label -> Set vertex
gnodes (vertexes, _, _, _) = vertexes

ginitialnode :: Agraph vertex label -> vertex
ginitialnode (_, v, _, _) = v

initialnode :: Agraph vertex label -> vertex
initialnode (_, v, _, _) = v

gtrans :: Agraph vertex label -> Set (Atrans vertex label)
gtrans (_, _, _, trans) = trans

gsource :: Atrans vertex label -> vertex
gsource (v, _, _) = v

glabel :: Atrans vertex label -> label
glabel (_, e, _) = e

gtarget :: Atrans vertex label -> vertex
gtarget (_, _, v) = v

isTerminal ::  Eq vertex => vertex -> Agraph vertex label -> Bool
isTerminal q (_,_,_,trxs) =
  let l = S.toList trxs in q € [q_ | (_,_,q_) <- l, L.all (\(q',_,_) -> q' /= q) l]

-- gstep gr v
--  PRE:  
--  POST: returns the outgoing edges of v in gr
goutgoing :: Eq vertex => Agraph vertex label -> vertex -> Set (Atrans vertex label)
goutgoing (_, _, _,trans) v = S.filter (\t -> v == (gsource t)) trans

-- gincoming gr
--  PRE:  
--  POST: builds a map associating to each vertex of gr its incoming edges
gincoming :: Eq vertex => Ord vertex => Agraph vertex label -> Map vertex (Set (Atrans vertex label))
gincoming (vertexes, _, _,trans) = M.fromList $ i
    where i = L.map (\v -> (v, S.filter (\t -> v == (gtarget t)) trans)) (S.toList vertexes)


-- gpath gr s t l
--   PRE:  s, t veterexes in gr, l list of veterexes of gr
--   POST: returns the list of (elementary) paths in gr from s to t not passing trough veterexes in l
gpath :: Eq vertex => (Show label) => (Show vertex) => Agraph vertex label -> vertex -> vertex -> [vertex] -> [[Atrans vertex label]]
gpath gr s t l
    | s == t    = if (s € l)
                  then []
                  else ([[]] ++ [tr:p | tr@(s0,_,s') <- s_outgoing, s0 /= s', p <- gpath gr s' t l])
    | otherwise = [tr:p | tr@(_, _, s') <- s_outgoing, p <- (paths s')]
    where s_outgoing = [(s0, e, s') | (s0, e, s') <- (S.toList $ goutgoing gr s), not(s' € l)]
          paths v = gpath gr v t ([s] ++ l)

-- cp current paths
gpath_ :: Eq vertex => Ord vertex => Eq label => (Show label) => (Show vertex) =>
          Agraph vertex label ->
              Map vertex (Set (Atrans vertex label)) ->
                  Map vertex [[Atrans vertex label]] ->
                      vertex ->
                          vertex ->
                              [Atrans vertex label] ->
                                  Map vertex [[Atrans vertex label]]
gpath_ gr incoming cp s t l
    | s == t    = cp
    | otherwise = addtr (S.toList $ incoming!t) l cp
    where addpath _ [] m     = m
          addpath c (p:ps) m = M.insert c (if c € M.keys m' then (cp!c ++ [p]) else [p]) m'
              where m' = addpath c ps m
          paths arc@(u, _, _) visited current = if arc € visited
                                                then current
                                                else gpath_ gr incoming current s u (arc:visited)
          addtr [] _ cp' = cp'
          addtr (tr@(v,_,_):trs) l' cp'
              | tr € l'    = addtr trs l' cp'
              | otherwise =  cp''
              where tov = paths tr l' cp'
                    tot = if M.null tov || not(v € M.keys tov) || [[]] == tov!v
                             then if v == s then [[tr]] else [[]]
                           else L.map (\p -> p ++ [tr]) (if v € M.keys tov then tov!v else [[]])
                    cp'' = addtr trs (tr:l') (addpath t tot tov)

gpath' :: Eq vertex => Ord vertex => Eq label => (Show label) => (Show vertex) =>
          Agraph vertex label ->
              Map vertex (Set (Atrans vertex label)) ->
                  Map vertex [[Atrans vertex label]] ->
                      vertex ->
                          [vertex] ->
                              [Atrans vertex label] ->
                                  Map vertex [[Atrans vertex label]]
gpath' gr incoming cp s trs l
    | L.null trs = cp
    | s == t     = gpath' gr incoming cp s (tail trs) l
    | L.null itr = gpath' gr incoming cp s (tail trs) l
    | otherwise  = gpath' gr incoming cp' s (v:trs) (tr:l)
    where t = head trs
          itr = (S.toList (incoming!t)) L.\\ l
          tr@(v, _, _)  = head itr
          cp' = case (v € M.keys cp) of
                  False -> addpath t [[tr]] cp
                  True  -> if [[]] == cp!v || L.null(cp!v)
                             then addpath t [[tr]] cp
                           else addpath t [p ++ [tr] | p <- cp!v] cp
          addpath _ [] m     = m
          addpath c (p:ps) m = M.insert c (if c € M.keys m' then (m'!c ++ [p]) else [p]) m'
              where m' = addpath c ps m
         
tokenifychar :: Char -> String
tokenifychar c = case c of
                 '_'  -> "UNDERSCORE"
                 '+'  -> "PLUS"
                 '-'  -> "MINUS"
                 '*'  -> "TIMES"
                 '='  -> "EQ"
                 '/'  -> "FWDSLASH"
                 '\\' -> "BCKSLASH"
                 '('  -> "ORBRA"
                 ')'  -> "CRBRA"
                 '['  -> "OSBRA"
                 ']'  -> "CSBRA"
                 '{'  -> "OCBRA"
                 '}'  -> "CCBRA"
                 '&'  -> "EEE"
                 '^'  -> "ACUTE"
                 '%'  -> "PRECENT"
                 '$'  -> "DOLLAR"
                 '#'  -> "SHARP"
                 '@'  -> "AT"
                 '!'  -> "EMARK"
                 '?'  -> "QMARK"
                 '±'  -> "PLUSMINUS"
                 '§'  -> "PARA"
                 '\'' -> "ACCENT"
                 ':'  -> "COLON"
                 ';'  -> "SEMICOLON"
                 '|'  -> "PAR"
                 '<'  -> "LT"
                 '>'  -> "GT"
                 ','  -> "COMMA"
                 '.'  -> "PERIOD"
                 '\"' -> "QUOTE"
                 '~'  -> "TILDE"
                 ' '  -> "BLANK"
                 _    -> [c]

tokenifymsg :: Message -> String
tokenifymsg msg = L.concat $ L.map tokenifychar msg

debug :: (Show a) => a -> b
debug x = error (show $ x)

mkFileName :: String -> String -> String -> String -> String
mkFileName suf dir fname ext =
  dir ++ fname ++ (rmChar '\"' suf) ++ ext

-- Some strings and an auxiliary function to handle node creation
lpref :: String
epref :: String
( lpref , epref ) = ( "*<" , ">*" )

newNode :: (Int -> Bool) -> Int -> Int -> Int
newNode excluded n j = if (excluded n) then n else (if n > 0 then n + j else n - j)


