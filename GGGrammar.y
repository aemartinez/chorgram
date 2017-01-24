--
--
-- Author: Emilio Tuosto <emilio@le.ac.uk>

-- A very basic grammar and parser for the textual editing of global graphs.
-- The grammar is the one used in the ICE16 paper with the addition
-- of the repeat-until construct:
--
--    G ::= P -> P : M | G|G | G+G | G;G | * G @ P | { G }
--
-- the binary operators |, +, and ; are given in ascending order of
-- precedence.  The parser generator is Haskell's 'Happy' and the
-- parser (GGparser.hs) is obtained by typing'make parser'. Note that
-- the empty graph has been removed as not necessary.
--
-- The only syntactic check made (right now) during the parsing are
-- (i) that sender and receiver of interactions have to be different,
-- (2) that the participant controlling a loop is active in the
-- loop. However, the error messages are still not informative.
--
-- Note: strings are made of the following characters
--
--   0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ()\\^_`abcdefghijklmnopqrstuvwxyz\167/$#&~,.
--
-- and must start with a letter when specifying the identity of a participant.
--
-- Text enclosd by '[' and ']' is treated as comment
--
-- TODO: improve parseError
-- TODO: add line numbers
--
{
module GGparser where
import SyntacticGlobalGraphs
import Data.List as L
import Data.Set as S
import Data.Map.Strict as M
import Misc
import CFSM
}

%name gggrammar
%tokentype { Token }
%error { parseError }
-- %monad { P }
-- %monad { Err } { thenErr } { returnErr }

%token
  str	        { TokenStr $$ }
  '§'	        { TokenEmp    }
  '='	     	{ TokenEqu    }
  '&'	     	{ TokenNwd    }
  '->'	     	{ TokenArr    }
  '=>'	        { TokenMAr    }
  '|'	        { TokenPar    }
  '+'	        { TokenBra    }
  ';'	        { TokenSeq    }
  '*'	        { TokenSta    }
  '@'   	{ TokenUnt    }
  ':'	        { TokenSec    }
  '{'	        { TokenBro    }
  '}'	        { TokenBrc    }
  ','	        { TokenCom    }
  '['	        { TokenCtb    }
  ']'	        { TokenCte    }
--  '\n'	        { TokenNln    }

%right '&'
%right '|'
%right '+'
%right ';'

%%

Gdec : G                        { [(self, $1)] }
     | D                        { checkCircularDeps $1 } -- there must be no circular dependencies; just macro expansion

D : NamedChor                   { [$1] }
  | NamedChor '&' D             { case (fst $1) € (L.map fst $3) of
                                    True  -> myErr("Bad name: " ++ (fst $1) ++ " multiply defined")
                                    False -> $1:$3
                                }

NamedChor : str '=' G           { if isPtp $1
                                  then ($1,$3)
                                  else myErr("Bad name: " ++ $1 ++ "is not a valid participant's name")
                                }

G : '§'				{ myErr "§ not permitted" } -- it used to be (Emps, S.empty) when designers where allowed to use §; 
  | str '->' str ':' str        { case ((isPtp $1), (isPtp $3), not($1 == $3)) of
				    (True, True, True)   -> ((Act ($1 , $3) $5), S.fromList [$1,$3], S.empty)
				    (True, False, True)  -> myErr ("Bad name: " ++ $3  ++ " is not a valid participant's name")
				    (True, _, False)     -> myErr ("A sender " ++ $3 ++ " cannot be also the receiver")
				    (False, True, True)  -> myErr ("Bad name: " ++ $1)
				    (False, False, True) -> myErr ("Bad names: " ++ $1 ++ " and " ++ $3 ++ " are not valid participant's names")
				    (False, _, False)    -> myErr ("Bad name: " ++ $1 ++ " and sender and receiver must be different")
                                }
  | str '=>' ptps ':' str       { case ((isPtp $1), not(L.elem $1 $3)) of
                                  (True, True)   -> case $3 of
                                                    []   -> myErr ($1 ++ " cannot be empty") -- ($1 ++ " => " ++ "[]")
                                                    s:[] -> ((Act ($1 , s) $5), S.fromList([$1,s]), S.empty)
                                                    _    -> (Par (L.map (\s -> (Act ($1 , s) $5)) $3), S.fromList($1:$3), S.empty)
                                  (True, False)  -> myErr ($1 ++ " must not be in " ++ (show $3))
                                  (False, _)     -> myErr ("Bad name: " ++ $1  ++ " is not a valid participant's name")
                                }
  | G '|' G  	     		{ (Par ((checkToken TokenPar $1) ++ (checkToken TokenPar $3)), S.union (ggroles $1) (ggroles $3), S.union (gginv $1) (gginv $3)) }
  | G '+' G       		{ (Bra (S.fromList $ (checkToken TokenBra $1) ++ (checkToken TokenBra $3)), S.union (ggroles $1) (ggroles $3), S.union (gginv $1) (gginv $3)) }
  | G ';' G  	     		{ (Seq ((checkToken TokenSeq $1) ++ (checkToken TokenSeq $3)), S.union (ggroles $1) (ggroles $3), S.union (gginv $1) (gginv $3)) }
  | '*' G '@' str               {let roles = ggroles $2 in
      				  case ((isPtp $4), (S.member $4 roles)) of
                                    (True, True)  -> (Rep (ggterm $2) $4 , S.union (S.singleton $4) roles, (gginv $2))
                                    (False, _)    -> myErr ("Bad name " ++ $4)
                                    (True, False) -> myErr ("Participant " ++ $4 ++ " is not in the loop")
                                }
  | '{' G '}'			{ ( $2 ) }
  | str                         { case isPtp $1 of
                                    True  -> ((Inv $1), S.empty, S.singleton $1)
                                    False -> myErr ("Bad name: " ++ $1  ++ " is not a valid participant's name")
                                }

ptps : str                      { if (isPtp $1) then [$1] else myErr ("Bad name: " ++ $1  ++ " is not a valid participant's name") }
  | str ',' ptps                { if (isPtp $1)
                                  then (case $3 of
                                        [] ->  [$1]
                                        (s:l) -> ($1:s:l))
                                  else myErr ("Bad name " ++ $1  ++ " is not a valid participant's name")
                                }


{
self :: String
self = "__self__"

data Token =
  TokenStr String
  | TokenPtps [Ptp]
  | TokenEqu
  | TokenNwd
  | TokenEmp
  | TokenArr
  | TokenPar
  | TokenBra
  | TokenSeq
  | TokenSta
  | TokenUnt
  | TokenSec
  | TokenBro
  | TokenBrc
  | TokenCom
  | TokenCtb
  | TokenCte
  | TokenMAr
  | TokenErr String
  deriving Show


-- lexer :: String -> [Token]
-- lexer :: (Token -> Err a) -> Err a
lexer s = case s of
    [] -> []
    '[':r     -> lexer $ tail (L.dropWhile (\c->c/=']') r)
    '.':'.':r -> lexer $ tail (L.dropWhile (\c->c/='\n') r)
    ' ' :r    -> lexer r
    '\t':r    -> lexer r
    '\n':r    -> lexer r
    '&' :r    -> TokenNwd : (lexer r)
    '=' :r    -> TokenEqu : (lexer r)
    '-':'>':r -> TokenArr : (lexer r)
    '=':'>':r -> TokenMAr : (lexer r)
    '§':r     -> TokenEmp : lexer r
    '|':r     -> TokenPar : lexer r
    '+':r     -> TokenBra : lexer r
    '*':r     -> TokenSta : lexer r
    '@':r     -> TokenUnt : lexer r
    ':':r     -> TokenSec : lexer r
    ';':r     -> TokenSeq : lexer r
    ',':r     -> TokenCom : lexer r
    '{':r     -> TokenBro : lexer r
    '}':r     -> TokenBrc : lexer r
    _         -> TokenStr (fst s') : (lexer $ snd s')
        where s' = span isAlpha s
    
parseError :: [Token] -> a
parseError err = case err of
                    TokenErr s:_ -> myErr s
                    _            -> myErr (show err)


-- parseError :: [Token] -> Err a
-- parseError tokens = failErr "Parse error"

--
-- Starting to plagiarise from Happy's user manual
-- (E has been renamed with Err beause it clashed with
-- the type of events)
--
data Err a = Ok a | Failed String

thenErr :: Err a -> (a -> Err b) -> Err b
m `thenErr` k = case m of
       		Ok a     -> k a
		Failed e -> Failed e

returnErr :: a -> Err a
returnErr a = Ok a

failErr :: String -> Err a
failErr err = Failed err

catchErr :: Err a -> (String -> Err a) -> Err a
catchErr m k = case m of
      		Ok a     -> Ok a
		Failed e -> k e

gginv :: (a,b,c) -> c
gginv (_,_,x) = x

ggroles :: (a,b,c) -> b
ggroles (_,x,_) = x

ggterm :: (a,b,c) -> a
ggterm (x,_,_) = x

checkCircularDeps :: [(String, (GG, Set Ptp ,Set String))] -> [(String, (GG, Set Ptp, Set String))]
checkCircularDeps ds = let env = M.fromList ds in
  case M.null env of
    True  -> ds
    False -> if noloops
             then ds
             else myErr("Sets of recursive definitions not permitted:\t" ++ (show checklist))
      where gnames = M.keys env
            noloops = not(L.foldr (||) False (L.map snd checklist))
            calltc g = let ginv = gginv (env!g) in (if S.null ginv then ginv else S.union ginv (S.unions (L.map calltc (S.toList ginv))))
            checklist = L.map (\x -> (x, S.member x (calltc x))) gnames
            


-- type LineNumber = Int

-- data ParseResult a = Ok a | Failed String
-- type P a = String -> ParseResult a


-- getLineNo :: P LineNumber
-- getLineNo = \s l -> Ok l

--
-- Plagiarism done
--
-- checkToken 'flattens', parallel, branching, and sequential composition
checkToken :: Token -> (GG, Set Ptp, Set String) -> [GG]
checkToken t (g,_,_) = case t of
                         TokenPar -> case g of
                                       Par l -> l
                                       _ -> [g]
                         TokenBra -> case g of
                                       Bra l -> S.toList l
                                       _ -> [g]
                         TokenSeq -> case g of
                                       Seq l -> l
                                       _ -> [g]
                         _        -> [g]

-- ggsptp computes the set of participants of a syntactic global graph
ggsptp :: Set Ptp -> GG -> Set Ptp
ggsptp ps g = case g of
               Emp         -> ps
               Act (s,r) _ -> S.union ps (S.fromList [s,r])
               Par gs      -> S.union ps (S.unions (L.map (ggsptp S.empty) gs))
               Bra gs      -> S.union ps (S.unions (L.map (ggsptp S.empty) (S.toList gs)))
               Seq gs      -> S.union ps (S.unions (L.map (ggsptp S.empty) gs))
               Rep g' p    -> S.union ps (ggsptp (S.singleton p) g')

myErr :: String -> a
myErr err = error ("sggparser: ERROR - " ++ err)
}
