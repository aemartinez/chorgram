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
type Edge vertex label = (vertex, label, vertex)
type Graph vertex label = (Set vertex, vertex, Set label, Set(Edge vertex label))

data Command = GMC | GG | SGG | GG2FSA | SYS | MIN | PROD | HGSEM
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

flatten :: [[a]] -> [a]
-- nomen omen
flatten ls =
  case ls of
    []    -> []
    l:lls -> [e | e <-l] ++ (flatten lls)

isAlpha :: Char -> Bool
-- For parsing: strings are made of the following characters
--
--   0123456789
--   <=>()\\^_`\167/$#&~,.
--   ABCDEFGHIJKLMNOPQRSTUVWXYZ
--   abcdefghijklmnopqrstuvwxyz
--
-- and must start with a letter when specifying the identity of a
-- machine (non-terminal Ptp) or of system.
isAlpha c = c € ([x | x <- ['0'.. 'z'] ++ ['/', '$', '#', '&', '~', '\"'],
                  not (x € ['@', '.', ',', ';', ':', '(', ')', '[', ']', '{', '}', '|', '+', '*', '!', '?', '-', '%', '§'])
                 ])

isPtp :: String -> Bool
-- Names of participants have to begin with a letter
isPtp s = (s /= []) && ( ((head s) € (['a'.. 'z'] ++ ['A'.. 'Z'])) && (L.all isAlpha (tail s)) )

idxOffset :: (Num a, Eq b) => b -> [b] -> a -> a
-- PRE:  
-- POST: -1 if e not in l, otherwise first position of e in the list plus the offset o
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


equivalenceRelation :: (Eq a) => [(a,a)] -> [(a,a)]
-- Assume that (x,x) in ys for all x
equivalenceRelation ys = transitiveClosure (L.nub $ addPairs ys)
    where addPairs ((x,y):xs) =  if x/=y
                                 then (x,y):((y,x):(addPairs xs))
                                 else (y,x):(addPairs xs)
          addPairs [] = []
        

equivalenceClass :: (Eq a, Ord a) => Set (a,a) ->  a -> Set a
equivalenceClass rel z = S.fold (S.union) S.empty $ 
                         S.map (\(x,y) -> S.insert x (S.singleton y)) (S.filter (\(x,y) -> x == z || y ==z) rel)
          
transitiveClosure :: (Eq a) =>  [(a, a)] -> [(a, a)]
-- http://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
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

mkSep :: [String] -> String -> String
mkSep l sep =
  case l of
    [] -> ""
    s:[] -> s
    "": l' -> mkSep l' sep
    s:l' -> s ++ sep ++ (mkSep l' sep)

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

usage :: Command -> String
-- Message on how to use a command
usage cmd = "Usage: " ++ msg
  where msg = case cmd of
               GMC   -> "gmc [-b | --bound number] [-l] [-m | --multiplicity number] [-sn] [-D detmode] [-d | --dir dirpath] [-fs | --fontsize fontsize] [-ts] [-cp cpattern] [-tp tpattern] [-v] filename \n   defaults: \t bound = 0 \n\t\t mutiplicity = 0 \n\t\t dirpath = " ++ dirpath ++ "\n\t\t fontsize = 8 \n\t\t cpattern = \"\" \n\t\t tpattern = \"- - - -\"\n\t\t detmode = no\n"
               GG    -> "BuildGlobal [-d | --dir dirpath] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               SGG   -> "sgg [-d dirpath] [-l] [--sloppy] filename [-rg]\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               GG2FSA-> "gg2fsa [-d dirpath] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               SYS   -> "systemparser [-d dirpath] [-v] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               MIN   -> "minimise [-D detmode] [-d dirpath] [-v] filename\n\t default: dirpath = " ++ dirpath ++ "\n\t\t  detmode = min\n"
               PROD  -> "cfsmprod [-d dirpath] [-l] filename\n\t default: \t dirpath = " ++ dirpath ++ "\n"
               HGSEM -> "hgsem [-d dirpath] [--sloppy] filename\n\t default: \t dirparth = " ++ dirpath ++ "\n"

msgFormat :: Command -> String -> String
msgFormat cmd msg =
  let pre = case cmd of
              GMC   -> "gmc:\t"
              GG    -> "gg:\t"
              SGG   -> "sgg:\t"
              GG2FSA-> "gg2fsa:\t"
              SYS   -> "systemparser:\t"
              MIN   -> "minimise:\t"
              PROD  -> "cfsmprod:\t"
              HGSEM -> "hgsem:\t"

  in pre ++ msg


myPrint :: Map String String -> Command -> String -> IO ()
myPrint flags cmd msg = if (flags!"-v" == "v") then putStrLn $ msgFormat cmd msg else return ()

defaultFlags :: Command -> Map String String
-- The default argument of each command
defaultFlags cmd = case cmd of
                     GMC   -> M.fromList [("-d",dirpath),
                                          ("-v",""),
                                          ("-m","0"),   -- multiplicity **deprecated**
                                          ("-ts",""),
                                          ("-b","0"),
                                          ("-cp", ""),
                                          ("-tp", "- - - -"),
                                          ("-p", ""),
                                          ("-D","no")    -- 'min' for minimisation, 'det' for determinisation, 'no' for nothing
                                         ]
                     GG    -> M.fromList [("-d",dirpath), ("-v","")]
                     SGG   -> M.fromList [("-d",dirpath), ("-v","")]
                     GG2FSA-> M.fromList [("-d",dirpath), ("-v","")]
                     SYS   -> M.fromList [("-d",dirpath), ("-v","")]
                     MIN   -> M.fromList [("-d",dirpath), ("-v",""), ("-D","min")]
                     PROD  -> M.fromList [("-d",dirpath), ("-v","")]
                     HGSEM -> M.fromList [("-d",dirpath), ("-v","")]

getFlags :: Command -> [String] -> Map String String
getFlags cmd args =
  case cmd of
    GMC -> case args of
      []                 -> defaultFlags(cmd)
      "-l":xs            -> M.insert "-l" "no"    (getFlags cmd xs)
      "-rg":xs           -> M.insert "-rg" "rg"   (getFlags cmd xs) -- for reversible ggs TODO: add to manual
      "-ts":xs           -> M.insert "-ts" "ts"   (getFlags cmd xs)
      "-sn":xs           -> M.insert "-sn" "no"   (getFlags cmd xs)      
      "-D":y:xs          -> M.insert "-D"  y      (getFlags cmd xs)
      "-b":y:xs          -> M.insert "-b"  y      (getFlags cmd xs)
      "-m":y:xs          -> M.insert "-m"  y      (getFlags cmd xs)
      "--muliply":y:xs   -> M.insert "-m"  y      (getFlags cmd xs)
      "-d":y:xs          -> M.insert "-d"  y      (getFlags cmd xs)
      "--dir":y:xs       -> M.insert "-d"  y      (getFlags cmd xs)
      "--fontsize":y:xs  -> M.insert "-fs" y      (getFlags cmd xs)
      "-cp":y:xs         -> M.insert "-cp" y      (getFlags cmd xs)
      "-tp":y:xs         -> M.insert "-tp" y      (getFlags cmd xs)
      "-p":y:xs          -> M.insert "-p"  y      (getFlags cmd xs)
      "-v":xs            -> M.insert "-v"  "v"    (getFlags cmd xs)
      _                  -> error $ usage(cmd)
    GG  ->  case args of
      []        -> defaultFlags(cmd)
      "-d":y:xs -> M.insert "-d" y    (getFlags cmd xs)
      "-v":xs   -> M.insert "-v" "v"  (getFlags cmd xs)
      _         -> error $ usage(cmd)
    SGG -> case args of
      []            -> defaultFlags(cmd)
      "-d":y:xs     -> M.insert "-d"  y          (getFlags cmd xs)
      "-rg":xs      -> M.insert "-rg" "yes"      (getFlags cmd xs)
      "--sloppy":xs -> M.insert "--sloppy" "yes" (getFlags cmd xs)
      _             -> error $ usage(cmd)
    GG2FSA -> case args of
      []            -> defaultFlags(cmd)
      "-d":y:xs     -> M.insert "-d"  y    (getFlags cmd xs)
      _             -> error $ usage(cmd)
    SYS -> case args of
      []        -> defaultFlags(cmd)
      "-d":y:xs -> M.insert "-d"  y   (getFlags cmd xs)
      "-v":xs   -> M.insert "-v" "v"  (getFlags cmd xs)
      _         -> error $ usage(cmd)
    MIN -> case args of
      []        -> defaultFlags(cmd)
      "-d":y:xs -> M.insert "-d"  y   (getFlags cmd xs)
      "-v":xs   -> M.insert "-v" "v"  (getFlags cmd xs)
      "-D":y:xs -> M.insert "-D"  y   (getFlags cmd xs)
      _         -> error $ usage(cmd)
    PROD -> case args of
      []        -> defaultFlags(cmd)
      "-d":y:xs -> M.insert "-d"  y    (getFlags cmd xs)
      _         -> error $ usage(cmd)
    HGSEM -> case args of
      []            -> defaultFlags(cmd)
      "--sloppy":xs -> M.insert "--sloppy" "yes" (getFlags cmd xs)
      "-d":y:xs     -> M.insert "-d"  y          (getFlags cmd xs)
      _         -> error $ usage(cmd)

--
-- Some utilities on graphs
--


pClosure :: Ord vertex => Ord label => Graph vertex label -> (label -> Bool) -> vertex -> Set vertex
--  PRE:
--  POST: returns the closure of vertexes reachable from v with
--        transitions that satisfy the predicate lpred on labels
pClosure g lpred v =
  let ptrans = S.filter (\(_, l, _) -> (lpred l)) (edgesOf g)
      aux res wl visited =
        case wl of
          []     -> S.insert v res
          v':wl' -> if v' € visited
                    then aux res wl' visited
                    else aux res' wl'' (v':visited)
            where vs'  = S.foldl S.union S.empty (S.map (\(q,_,q') -> if q == v' then (S.singleton q') else S.empty) ptrans)
                  res' = S.union res vs'
                  wl'' = wl' ++ S.toList vs'
  in aux S.empty [v] []


pRemoval :: Ord vertex => Ord label => Graph vertex label -> (label -> Bool) -> Graph vertex label
-- Generalisation of the epsilon-removal in NFA to p-removal for a predicate lpred on labels
pRemoval g@(vs, v0, labels, trxs) lpred = (vs, v0, S.map glabel trxs', trxs')
  where
    -- TODO: computing vmap like this is quite inefficient and should be optimised
    vmap = M.fromList $ L.zip (S.toList vs) (L.map (pClosure g lpred) (S.toList vs))
    (lpred_trxs, other_trxs) = S.partition (\(_, l, _) -> lpred l) trxs
    aux = \(s, l, t) -> S.map (\q -> (q, l, t)) (vmap!s)
    new_trxs = S.unions $ S.toList $ S.map aux other_trxs
    trxs' = S.difference new_trxs lpred_trxs


reachableVertexes :: Ord vertex => Ord label => Graph vertex label -> vertex -> Set vertex
--  PRE:
--  POST: returns the set of vertexes of g reachable from a given vertex
reachableVertexes g = pClosure g (\_ -> True)


-- Projections of Graph and Edge components

gnodes :: Graph vertex label -> Set vertex
gnodes (vertexes, _, _, _) = vertexes

ginitialnode :: Graph vertex label -> vertex
ginitialnode (_, v, _, _) = v

initialnode :: Graph vertex label -> vertex
initialnode (_, v, _, _) = v

edgesOf :: Graph vertex label -> Set (Edge vertex label)
edgesOf (_, _, _, trans) = trans

gsource :: Edge vertex label -> vertex
gsource (v, _, _) = v

glabel :: Edge vertex label -> label
glabel (_, l, _) = l

gtarget :: Edge vertex label -> vertex
gtarget (_, _, v) = v

-- Some utilities

grenameVertex :: Ord vertex => Ord label => Map vertex vertex -> Graph vertex label -> Graph vertex label
grenameVertex sigma (nodes, n0, labels, trans) = (nodes', n0', labels, trans')
  where nodes' = S.map aux nodes
        n0'    = aux n0
        trans' = S.map (\(n, e, n') -> (aux n, e, aux n')) trans
        aux n  = if M.member n sigma then sigma!n else n

isTerminal ::  Eq vertex => vertex -> Graph vertex label -> Bool
isTerminal q (_,_,_,trxs) =
  let l = S.toList trxs in q € [q_ | (_,_,q_) <- l, L.all (\(q',_,_) -> q' /= q) l]

goutgoing :: Eq vertex => Graph vertex label -> vertex -> Set (Edge vertex label)
-- goutgoing gr v
--  PRE:  v is a vertex of gr
--  POST: returns the outgoing edges of v in gr
goutgoing (_, _, _,trans) v = S.filter (\t -> v == (gsource t)) trans

gincoming :: Eq vertex => Ord vertex => Graph vertex label -> Map vertex (Set (Edge vertex label))
-- gincoming gr
--  PRE:  
--  POST: builds a map associating to each vertex of gr its incoming edges
gincoming (vertexes, _, _,trans) = M.fromList $ i
    where i = L.map (\v -> (v, S.filter (\t -> v == (gtarget t)) trans)) (S.toList vertexes)


gpath :: Eq vertex => (Show label) => (Show vertex) => Graph vertex label -> vertex -> vertex -> [vertex] -> [[Edge vertex label]]
-- gpath gr s t l
--   PRE:  s, t veterexes in gr, l list of veterexes of gr
--   POST: returns the list of (elementary) paths in gr from s to t not passing trough veterexes in l
gpath gr s t l
    | s == t    = if (s € l)
                  then []
                  else ([[]] ++ [tr:p | tr@(s0,_,s') <- s_outgoing, s0 /= s', p <- gpath gr s' t l])
    | otherwise = [tr:p | tr@(_, _, s') <- s_outgoing, p <- (paths s')]
    where s_outgoing = [(s0, e, s') | (s0, e, s') <- (S.toList $ goutgoing gr s), not(s' € l)]
          paths v = gpath gr v t ([s] ++ l)

gpath_ :: Eq vertex => Ord vertex => Eq label => (Show label) => (Show vertex) =>
          Graph vertex label ->
              Map vertex (Set (Edge vertex label)) ->
                  Map vertex [[Edge vertex label]] ->
                      vertex ->
                          vertex ->
                              [Edge vertex label] ->
                                  Map vertex [[Edge vertex label]]
-- cp current paths
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
          Graph vertex label ->
              Map vertex (Set (Edge vertex label)) ->
                  Map vertex [[Edge vertex label]] ->
                      vertex ->
                          [vertex] ->
                              [Edge vertex label] ->
                                  Map vertex [[Edge vertex label]]
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


underscore :: String
underscore = "UNDERSCORE"

plus :: String
plus = "PLUS"

minus :: String
minus = "MINUS"

times :: String
times = "TIMES"

eq :: String
eq = "EQ"

fwdslash :: String
fwdslash = "FWDSLASH"

bckslash :: String
bckslash = "BCKSLASH"

orbra :: String
orbra = "ORBRA"

crbra :: String
crbra = "CRBRA"

osbra :: String
osbra = "OSBRA"

csbra :: String
csbra = "CSBRA"

ocbra :: String
ocbra = "OCBRA"

ccbra :: String
ccbra = "CCBRA"

eee :: String
eee = "EEE"

acute :: String
acute = "ACUTE"

precent :: String
precent = "PRECENT"

dollar :: String
dollar = "DOLLAR"

sharp :: String
sharp = "SHARP"

at :: String
at = "AT"

emark :: String
emark = "EMARK"

qmark :: String
qmark = "QMARK"

plusminus :: String
plusminus = "PLUSMINUS"

para :: String
para = "PARA"

accent :: String
accent = "ACCENT"

colon :: String
colon = "COLON"

semicolon :: String
semicolon = "SEMICOLON"

par :: String
par = "PAR"

lt :: String
lt = "LT"

gt :: String
gt = "GT"

comma :: String
comma = "COMMA"

period :: String
period = "PERIOD"

quote :: String
quote = "QUOTE"

tilde :: String
tilde = "TILDE"

blank :: String
blank = "BLANK"

tokenifychar :: Char -> String
tokenifychar c = case c of
  '_'  -> underscore
  '+'  -> plus
  '-'  -> minus
  '*'  -> times
  '='  -> eq
  '/'  -> fwdslash
  '\\' -> bckslash
  '('  -> orbra
  ')'  -> crbra
  '['  -> osbra
  ']'  -> csbra
  '{'  -> ocbra
  '}'  -> ccbra
  '&'  -> eee
  '^'  -> acute
  '%'  -> precent
  '$'  -> dollar
  '#'  -> sharp
  '@'  -> at
  '!'  -> emark
  '?'  -> qmark
  '±'  -> plusminus
  '§'  -> para
  '\'' -> accent
  ':'  -> colon
  ';'  -> semicolon
  '|'  -> par
  '<'  -> lt
  '>'  -> gt
  ','  -> comma
  '.'  -> period
  '\"' -> quote
  '~'  -> tilde
  ' '  -> blank
  _    -> [c]

-- change to String -> String to replace substrings with the right symbol
-- detokenifychar :: String -> String

detokenifyTable :: [(String,String)]
detokenifyTable = [
  (underscore ,  "_"),
  (plus , "+"),
  (minus , "-"),
  (times , "*"),
  (eq , "="),
  (fwdslash , "/"),
  (bckslash , "\\"),
  (orbra , "("),
  (crbra , ")" ),
  (osbra , "["),
  (csbra , "]"),
  (ocbra , "{"),
  (ccbra , "}"),
  (eee , "&" ),
  (acute , "^"),
  (precent , "%"),
  (dollar , "$"),
  (sharp , "#"),
  (at , "@"),
  (emark , "!"),
  (qmark , "?"),
  (plusminus , "±"),
  (para , "§"),
  (accent , "\""),
  (colon , ":"),
  (semicolon , ";"),
  (par , "|"),
  (lt , "<"),
  (gt , ">"),
  (comma , ","),
  (period , "."),
  (quote , "\""),
  (tilde , "~"),
  (blank , " ")
  ]

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
(lpref, epref) = ( "*<" , ">*" )

newNode :: (Int -> Bool) -> Int -> Int -> Int
newNode excluded n j = if (excluded n) then n else (if n > 0 then n + j else n - j)

