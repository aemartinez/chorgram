--
--
-- Author: Emilio Tuosto <emilio@le.ac.uk>

-- A very basic grammar and parser for the textual editing of CFSMs:
--
--    S ::= "system" str "of" Ptp ',' ... ',' Ptp ':' Ptp '=' M "||" ... "||" Ptp '=' M
--    M ::= .
--       |  p ';' M
--       |  p "do" Ptp
--       |  M + M
----       |  M | M
--       |  * M
--    p ::= Ptp '!' str | Ptp '?' str | "tau" str
--
-- operator + takes precedence over |. The parser generator is
-- Haskell's 'Happy' and text enclosd by '[' and ']' is treated as
-- comment.
--
-- The parser (CFSMparser.hs) is obtained by typing 'make parser'.
--
-- The only syntactic checks made (right now) during the parsing are
--
--   (i) that sender and receiver of interactions have to be different,
--   (ii) that defining equations are unique,
--   (iii) that communication actions name existing machines, and
--   (iv) a machine cannot be defined as its own dual.
--
-- However, the error messages are still not informative.
--
-- TODO: improve parseError
-- TODO: add line numbers
--

{
module SystemParser where
import CFSM
import Misc
import Data.List as L
import Data.Set as S
import Data.Map.Strict as M
}

%name sysgrammar
%tokentype { Token }
%error { parseError }

%token
  str	        { TokenStr $$ }
  '='	        { TokenEqu    }
  "||"	        { TokenDec    }
  ':'	        { TokenBod    }
  '!'	        { TokenSnd    }
  '?'	     	{ TokenRcv    }
  '|'	        { TokenPar    }
  '+'	        { TokenBra    }
  '*'	        { TokenSta    }
  ';'	        { TokenPre    }
  ','	        { TokenCom    }
  '('	        { TokenOrb    }
  ')'	        { TokenCrb    }
  '['	        { TokenCtb    }
  ']'	        { TokenCte    }
  "tau"         { TokenTau    }
  "do"          { TokenJmp    }
  "system"      { TokenSys    }
  "of"          { TokenDef    }
  "end"         { TokenStp    }
  
%right    '||'
%right    '|'
%right    '+'
%nonassoc "do"
%right    ';' ','
%nonassoc '*' '@'
%nonassoc '!' '?'

%%

Sys : "system" str "of" ptps ':' Mdec { if isPtp $2
                                        then let (sys, ptps) = systemFrom $4 $6 in (L.map (simpleStates 0 "") sys, ptps)
                                        else myErr "Name of system invalid: it must start with a letter"
                                      }

Mdec : D                      	      { [$1] }
     | D "||" Mdec                    { ($3 ++ [$1])
                                        -- if (fst $1) € (L.map fst $3)
                                        -- then myErr "Two equations for " ++ (fst $1)
                                        -- else ($3 ++ [$1])
                                      }

D : str '=' M                         { if isPtp $1
--                                  	then checkSelf($1, simpleStates 0 $3)
                                  	then
                                          let m = simpleStates 0 $1 (renamePtp self $1 $3)
                                          in checkSelf($1, replaceState (initialOf m) (statePref:$1) m)
                                  	else myErr ("Bad name " ++ $1)
                                      }

-- TODO: let | only without 'do'
-- M : par                               { cfsmProd $1 }
--   | '*' par                           { cfsmLoop $ cfsmProd $2 }

-- par : cho                             { let l = branch 0 $1 in [choice (initialOf $ head l) l] }
--     | par '|' cho                     { let l = branch 0 $3 in (choice (initialOf $ head l) l) : $1 }

M : cho                               { choice $1 }
  | '*' cho                           { cfsmLoop $ choice $2 }

cho : B  	     		      { [$1] }
    | cho '+' B                       { $1 ++ [$3] }

--B : pre ';' par                       { cfsmPref $1 (cfsmProd $3) }
B : pre ';' cho                       { cfsmPref $1 (choice $3) }
  | pre ';' "end"                     { cfsmPref $1 (emptyMachine 0) }
  | pre                               { cfsmPref $1 (emptyMachine 0) }
  | pre ';'                           { cfsmPref $1 (emptyMachine 0) }
  | pre "do" str                      { cfsmJump $1 $3 }
  | '(' M ')'                         { $2 }

pre : str '!' str		      { (Send,(self,$1),$3) }
    | str '?' str                     { (Receive,($1,self),$3) }
    | "tau" str                       { (Tau,(self,self),$2) }
    | "tau"                           { (Tau,(self,self),"") }

ptps : str                            { if (isPtp $1) then [$1] else myErr ("Bad name " ++ $1) }
     | str ',' ptps	              { if (isPtp $1) then ($1: $3) else myErr ("Bad name " ++ $1) }


{

stateChar :: Char
stateChar = '_'

statePref :: Char
statePref = 'q'

-- A useful type to handle the generation of new states in machines:
-- states are string representations of integers
type PState = Int

self :: String
self = "__self__"

data Token =
  TokenStr String
  | TokenEqu
  | TokenDec
  | TokenBod
  | TokenSnd
  | TokenRcv
  | TokenPar
  | TokenBra
  | TokenSta
  | TokenPre
  | TokenCom
  | TokenOrb
  | TokenCrb
  | TokenCtb
  | TokenCte
  | TokenTau
  | TokenJmp
  | TokenSys
  | TokenDef
  | TokenAs
  | TokenStp
  | TokenErr String
  deriving Show


lexer s = case s of
    [] -> []
    '[':r                     -> lexer $ tail (L.dropWhile (\c->c/=']') r)
    '.':'.':r                 -> lexer $ tail (L.dropWhile (\c->c/='\n') r)
    ' ' :r                    -> lexer r
    '\n':r                    -> lexer r
    '\t':r                    -> lexer r
    '|':'|':r                 -> TokenDec     :lexer r
    's':'y':'s':'t':'e':'m':r -> TokenSys    : lexer r
    'o':'f':r                 -> TokenDef    : lexer r
    'e':'n':'d':r             -> TokenStp    : lexer r
    't':'a':'u':r             -> TokenTau    : lexer r
    'd':'o':r                 -> TokenJmp    : lexer r
    '=':r                     -> TokenEqu    : lexer r
    ':':r                     -> TokenBod    : lexer r
    '|':r                     -> TokenPar    : lexer r
    '+':r                     -> TokenBra    : lexer r
    '*':r                     -> TokenSta    : lexer r
    '!':r                     -> TokenSnd    : lexer r
    '?':r                     -> TokenRcv    : lexer r
    ',':r                     -> TokenCom    : lexer r
    ';':r                     -> TokenPre    : lexer r
    '(':r                     -> TokenOrb    : lexer r
    ')':r                     -> TokenCrb    : lexer r
    _                         -> TokenStr s1 : (lexer $ s2)
        where (s1,s2) = span isAlpha s

parseError :: [Token] -> a
parseError err = case err of
                    TokenErr s:_ -> myErr s
                    _            -> myErr (show err)

  --
  -- Pre: d is the list of 'main' participants declared after the keyword 'of' in the production for Sys
  --      l is the list of pairs name-machine built from the parsing of Mdec
  --      mysys is initially empty and accumulates the machines for the identities in d
  -- Post: the result produces the system made of the machines in d without the auxiliary declarations.
  --
systemFrom :: [Ptp] -> [(Ptp,CFSM)] -> System
systemFrom d l =
  let mymap        = M.fromList l
      (roles, aux) = M.partitionWithKey (\p _ -> (p € d)) mymap
      getSys       = \(sys, ptps) -> (L.map (\i -> rmdo (ptps!i) S.empty (sys!!i)) (range $ L.length sys), ptps)
      addm         = \trxs added -> case trxs of
                                     []  -> added
                                     t:r -> let q' = tail $ gtarget t in
                                            if q' € (M.keys aux) && S.notMember q' added
                                            then addm (r++(S.toList (transitionsOf (aux!q')))) (S.insert q' added)
                                            else addm r added
      rmdo         = \p l m@(_, _, _,trxs) ->
                       let add = addm (S.toList trxs) S.empty -- S.filter (\p' -> p' € (M.keys aux) && S.notMember p' l) (S.map (\(_,_,q) -> (L.tail q)) trxs)
                       in cfsmUnion (initialOf m) (m:(L.map (\p' -> renamePtp p' p (aux!p')) (S.toList add)))
      preSys       = \pairs mysys@(sys,ptps) ->
                       case pairs of
                        []       -> mysys
                        (p,m):ls -> case (p € (M.keys roles), p € (M.keys aux)) of
                                     (True, False) -> (sys'++[rmdo p S.empty m], M.insert (L.length sys') p ptps')
                                       where (sys', ptps') = preSys ls mysys
                                     (False, True) -> preSys ls mysys
                                     _             -> myErr ("Machine " ++ p ++ " not declared")
  in getSys $ preSys l ([],M.empty)


-- checkSelf(ptp,m) checks that ptp does not communicate with itself
checkSelf :: (Ptp,CFSM) -> (Ptp,CFSM)
checkSelf (ptp, m@(states,q0,acts,trxs)) =
  let check act = case act of
                   (Send,(s,r),_)    -> (ptp == r)
                   (Receive,(s,r),_) -> (ptp == s)
                   (Tau,_,_)         -> False
   in if S.null $ S.filter check acts
      then (ptp,m)
      else myErr ("Machine " ++ ptp ++ " cannot communicate with itself: " ++ (show m))

mkstate :: PState -> State
mkstate i = stateChar:(show i)

emptyMachine :: PState -> CFSM
emptyMachine i = replaceState (initialOf emptyCFSM) (mkstate i) emptyCFSM

simpleStates :: Int -> Ptp -> CFSM -> CFSM
simpleStates offset p (states, q0, acts, trxs) =
  (S.map aux states, aux q0, acts, S.map (\(q,act,q') -> (aux q, act, aux q')) trxs)
  where pos = M.fromList ([(S.elemAt i states , i + offset) | i <- range $ S.size states])
        aux = \q -> if (q!!0 /= stateChar)
                    then (if q!!0 /= statePref then statePref:q else q)
                    else (mkstate (pos!q)++p)

branch :: Int -> [CFSM] -> [CFSM]
branch offset l =
  case l of
   []   -> []
   m:ls -> m':(branch (offset + (S.size $ statesOf m') + 1) ls)
     where m' = simpleStates offset "" m

choice :: [CFSM] -> CFSM
choice l =
  case l of
   []  -> emptyCFSM
   [m] -> m
   _   -> cfsmUnion q0 (L.map (\m -> replaceState (initialOf m) q0 m) (branch 0 l))
     where q0 = initialOf $ head l

cfsmPref :: Action -> CFSM -> CFSM
cfsmPref act m@(states, q0, acts, trxs) =
  let q0 = mkstate 0 in
    (S.insert q0 states', q0, S.insert act acts', S.insert (q0, act, q0') trxs')
      where (states', q0',acts',trxs') = simpleStates 1 "" m

cfsmJump :: Action -> Ptp -> CFSM
cfsmJump act q =
  let q0 = mkstate 0 in
    (S.insert q0 $ S.singleton q, q0, S.singleton act, S.singleton (q0, act, q))

cfsmLoop :: CFSM -> CFSM
cfsmLoop m@(states, q0, acts, trxs) =
  (S.filter (\q -> not(isTerminal q m)) states, q0, acts, S.map aux trxs)
    where aux (q,act,q') = (q,act, if isTerminal q' m then q0 else q')

myErr :: String -> a
myErr err = error ("sysparser: ERROR - " ++ err)
}
