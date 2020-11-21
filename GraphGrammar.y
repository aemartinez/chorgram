--
--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>

-- A very basic grammar and parser for the textual editing of global graphs.
-- The grammar is the one used in the ICE16 paper with the addition
-- of the repeat-until construct:
--
--    G ::= (o) | P -> P : M | G|G | G+G | G;G | * G @ P | repeat P { G } | ( G ) 
--
-- where '(o)' has a special role: it is the empty graph outside
-- loops, while in loops it marks a point where the selector may exit
-- the iteration. The binary operators |, +, and ; are given in
-- ascending order of precedence.  The parser generator is Haskell's
-- 'Happy' and the parser (GGparser.hs) is obtained by typing'make
-- parser'. Note that the empty graph has been removed as not
-- necessary. We have that syntaxes repeat P { G } and * G @ P are
-- equivalent.
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
  '->'	     	{ TokenArr    }
  '=>'	        { TokenMAr    }
  '|'	        { TokenPar    }
  '+'	        { TokenBra    }
  '*'	        { TokenSta    }
  ';'	        { TokenSeq    }
  '@'   	{ TokenUnt    }
  ':'	        { TokenSec    }
  '('	        { TokenBro    }
  ')'	        { TokenBrc    }
  ','	        { TokenCom    }
  '{'	        { TokenCurlyo }
  '}'	        { TokenCurlyc }
  'sel'         { TokenarB    }
  'repeat'      { TokenSta    }
  'end'         { TokenEnd    }
  'unless'      { TokenUnl    }
  '(o)'         { TokenEmp    }

%right '|'
%right '+'
%right ';'

%%

G : str '->' str ':' Msg        { case ((isPtp $1), (isPtp $3), not($1 == $3)) of
				    (True, True, True)   -> ((Act ($1 , $3) $5), S.fromList [$1,$3])
				    (True, False, True)  -> myErr ("Bad name " ++ $3)
				    (True, True, False)  -> myErr ("A sender " ++ $3 ++ " cannot be also the receiver in an interaction")
				    (_, False, False)    -> myErr ("Whaaat??? Sender " ++ $1 ++ " and receiver " ++ $3 ++ " are equal AND different!!!")
				    (_, True, True)      -> myErr ("Whaaat??? Sender " ++ $1 ++ " and receiver " ++ $3 ++ " are equal AND different!!!")
				    (False, False, True) -> myErr ("Bad names " ++ $1 ++ " and " ++ $3)
				    (False, _, False)    -> myErr ("Bad name " ++ $1 ++ " and sender and receiver must be different")
                                }
  | str '=>' ptps ':' Msg       { case ((isPtp $1), not(L.elem $1 $3)) of
                                  (True, True)   -> case $3 of
                                                    []   -> myErr ($1 ++ " cannot be empty") -- ($1 ++ " => " ++ "[]")
                                                    s:[] -> ((Act ($1 , s) $5), S.fromList([$1,s]))
                                                    _    -> (Par (L.map (\s -> (Act ($1 , s) $5)) $3),S.fromList($1:$3))
                                  (True, False)  -> myErr ($1 ++ " must be in " ++ (show $3))
                                  (False, _)     -> myErr ("Bad name " ++ $1)
                                }
  | G '|' G  	     		{ (Par ((checkToken TokenPar $1) ++ (checkToken TokenPar $3)), S.union (snd $1) (snd $3)) }
  | G '+' G       		{ (Bra (S.fromList $ (checkToken TokenBra $1) ++ (checkToken TokenBra $3)), S.union (snd $1) (snd $3)) }
  | G ';' G  	     		{ (Seq ((checkToken TokenSeq $1) ++ (checkToken TokenSeq $3)), S.union (snd $1) (snd $3)) }
  | '*' G '@' str               {
      				  case ((isPtp $4), (S.member $4 (snd $2))) of
                                    (True, True)  -> (Rep (fst $2) $4 , S.union (S.singleton $4) (snd $2))
                                    (False, _)    -> myErr ("Bad name " ++ $4)
                                    (True, False) -> myErr ("Participant " ++ $4 ++ " is not in the loop")
                                }
  | 'repeat' str '{' G '}'      {
      				  case ((isPtp $2), (S.member $2 (snd $4))) of
                                    (True, True)  -> (Rep (fst $4) $2 , S.union (S.singleton $2) (snd $4))
                                    (False, _)    -> myErr ("Bad name " ++ $2)
                                    (True, False) -> myErr ("Participant " ++ $2 ++ " is not in the loop")
                                }
  | '(' G ')'			{ ( $2 ) }
  | '{' G '}'			{ ( $2 ) }
  | '(o)'                       { (Emp, S.empty) }
--  | '§'				{ (Emp, S.empty) }

Msg : str                         { $1 }
    | str'(' ')'                  { $1 + '()' }
    | str'(' str ')'              { $1 + '(' + $3 + ')' }

guard : str ':' Msg '~'              
      |                           { M.empty }
      
ptps : str                      { if (isPtp $1) then [$1] else myErr ("Bad name " ++ $1) }
     | str ',' ptps             { if (isPtp $1)
                                  then (case $3 of
                                        [] ->  [$1]
                                        (s:l) -> ($1:s:l))
                                  else myErr ("Bad name " ++ $1)
                                }


{
data Token =
  TokenErr String
  | TokenStr String
  | TokenPtps [Ptp]
  | TokenGrd
  | TokenArr
  | TokenUnt 
  | TokenPar
  | TokenBra
  | TokenSeq
  | TokenSta
  | TokenBro
  | TokenBrc
  | TokenCom
  | TokenMAr
  | TokenCurlyo
  | TokenCurlyc
  | TokenEnd
  | TokenUnl
  | TokenEmp
  deriving Show


-- lexer :: String -> [Token]
-- lexer :: (Token -> Err a) -> Err a
lexer s = case s of
    []                        -> []
    '[':r                     -> lexer $ tail (L.dropWhile (\c->c/=']') r)
    '.':'.':r                 -> lexer $ tail (L.dropWhile (\c->c/='\n') r)
    ' '  :r                   -> lexer r
    '\n' :r                   -> lexer r
    '\t' :r                   -> lexer r
    '-':'>':r                 -> TokenArr : (lexer $ tail r)
    '=':'>':r                 -> TokenMAr : (lexer $ tail r)
    'e':'n':'d':r             -> TokenEnd : (lexer $ tail r)
    '§':r                     -> TokenEmp : lexer r
    '|':r                     -> TokenPar : lexer r
    '+':r                     -> TokenBra : lexer r
    's':'e':'l':r             -> TokenEmp : lexer r
    '*':r                     -> TokenSta : lexer r
    'r':'e':'p':'e':'a':'t':r -> TokenSta : (lexer $ tail r)
    '(':'o':')':r             -> TokenEmp : lexer r
    '@':r                     -> TokenUnt : lexer r
    ':':r                     -> TokenSec : lexer r
    ';':r                     -> TokenSeq : lexer r
    ',':r                     -> TokenCom : lexer r
    '(':r                     -> TokenBro : lexer r
    ')':r                     -> TokenBrc : lexer r
    '{':r                     -> TokenCurlyo : lexer r
    '}':r                     -> TokenCurlyc : lexer r
    _                         -> TokenStr (fst s') : (lexer $ snd s')
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



-- type LineNumber = Int

-- data ParseResult a = Ok a | Failed String
-- type P a = String -> ParseResult a


-- getLineNo :: P LineNumber
-- getLineNo = \s l -> Ok l

--
-- Plagiarism done
--
-- checkToken 'flattens', parallel, branching, and sequential composition
checkToken :: Token -> (GG, Set Ptp) -> [GG]
checkToken t (g,_) = case t of
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
