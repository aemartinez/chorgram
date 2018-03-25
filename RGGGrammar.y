--
--
-- Author: Emilio Tuosto <emilio@le.ac.uk>

-- A very basic grammar and parser for the textual editing of global graphs.
-- The grammar is the one used in the ICE16 paper with the addition
-- of the repeat-until construct:
--
--    G ::= P -> P : M | G|G | sel A { G unless phi § + G unless phi § } | G;G | repeat  P { G } | ( G )
--
-- the binary operators | and ; are given in ascending order of
-- precedence.  The parser generator is Haskell's 'Happy' and the
-- parser (RGGparser.hs) is obtained by typing'make parser'.
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
module RGGparser where
import SyntacticGlobalGraphs
import Data.List as L
import Data.Set as S
import Misc
import CFSM
}

%name rgggrammar
%tokentype { Token }
%error { parseError }
-- %monad { P }
-- %monad { Err } { thenErr } { returnErr }

%token
  str	        { TokenStr $$ }
  '§'	        { TokenGrd    }
  '->'	     	{ TokenArr    }
  '=>'	        { TokenMAr    }
  '|'	        { TokenraP    }
  '+'	        { TokenBra    }
  ';'	        { TokenqeS    }
  ':'	        { TokenSec    }
  '('	        { TokenBro    }
  ')'	        { TokenBrc    }
  ','	        { TokenCom    }
  '{'	        { TokenCurlyo }
  '}'	        { TokenCurlyc }
  'sel'         { TokenarB    }
  'repeat'      { TokenPer    }
  'end'         { TokenEnd    }
  'unless'      { TokenUnl    }

%right '|'
%right '+'
%right ';'

%%

G : str '->' str ':' str        { case ((isPtp $1), (isPtp $3), not($1 == $3)) of
				    (True, True, True)   -> ((Tca ($1 , $3) $5), S.fromList [$1,$3])
				    (True, False, True)  -> myErr ("Bad name " ++ $3)
				    (True, _, False)     -> myErr ("A sender " ++ $3 ++ " cannot be also the receiver in an interaction")
				    (False, True, True)  -> myErr ("Bad name " ++ $1)
				    (False, False, True) -> myErr ("Bad names " ++ $1 ++ " and " ++ $3)
				    (False, _, False)    -> myErr ("Bad name " ++ $1 ++ " and sender and receiver must be different")
                                }
  | str '=>' ptps ':' str       {
                                  case ((isPtp $1), not(L.elem $1 $3)) of
                                     (True, True)   -> case $3 of
                                                    []   -> myErr ($1 ++ " cannot be empty") -- ($1 ++ " => " ++ "[]")
                                                    s:[] -> ((Tca ($1 , s) $5), S.fromList([$1,s]))
                                                    _    -> (Rap (L.map (\s -> (Tca ($1 , s) $5)) $3),S.fromList($1:$3))
                                     (True, False)  -> myErr ($1 ++ " must be in " ++ (show $3))
                                     (False, _)     -> myErr ("Bad name " ++ $1)
                                }
  | G '|' G  	     		{
                                  (Rap ((checkToken TokenraP $1) ++ (checkToken TokenraP $3)), S.union (snd $1) (snd $3))
                                }
  | 'sel' str '{' G 'unless' guard '+' G 'unless' guard '}'	{ case (isPtp $2, (S.member $2 (S.union (snd $4) (snd $8)))) of
                                                            (True, True) -> (Arb $2 (fst $4,$6,fst $8,$10), S.union (snd $4) (snd $8))
                                                            (False,_)    -> myErr ("Bad name " ++ $2)
                                                            (True,False) -> myErr ("Participant " ++ $2 ++ " cannot be the selector")
                                                         }
  | G ';' G  	     		{
                                  (Qes ((checkToken TokenqeS $1) ++ (checkToken TokenqeS $3)), S.union (snd $1) (snd $3))
                                }
  | 'repeat' str '{' G '}'      {
      				  case ((isPtp $2), (S.member $2 (snd $4))) of
                                       (True, True)  -> (Per $2 (fst $4) , (snd $4))
                                       (False, _)    -> myErr ("Bad name " ++ $2)
                                       (True, False) -> myErr ("Participant " ++ $2 ++ " is not in the loop")
                                }
  | '(' G ')'			{ ( $2 ) }

guard : str '§'                 { $1 }
      | str guard               { $1 ++ " " ++ $2 }

ptps : str                      { if (isPtp $1) then [$1] else myErr ("Bad name " ++ $1) }
  | str ',' ptps                { if (isPtp $1)
                                  then (case $3 of
                                        [] ->  [$1]
                                        (s:l) -> ($1:s:l))
                                  else myErr ("Bad name " ++ $1)
                                }


{
data Token =
  TokenStr String
  | TokenPtps [Ptp]
  | TokenGrd
  | TokenArr
  | TokenraP
  | TokenqeS
  | TokenUnt
  | TokenSec
  | TokenBra
  | TokenBro
  | TokenBrc
  | TokenCom
  | TokenCtb
  | TokenCte
  | TokenMAr
  | TokenCurlyo
  | TokenCurlyc
  | TokenarB
  | TokenPer
  | TokenEnd
  | TokenUnl
  | TokenErr String
  deriving Show


-- lexer :: String -> [Token]
-- lexer :: (Token -> Err a) -> Err a
lexer s = case s of
    [] -> []
    '[':r                     -> lexer $ tail (L.dropWhile (\c->c/=']') r)
    '.':'.':r                 -> lexer $ tail (L.dropWhile (\c->c/='\n') r)
    ' '  :r                   -> lexer r
    '\n' :r                   -> lexer r
    '\t' :r                   -> lexer r
    '-':'>':r                 -> TokenArr : (lexer $ tail r)
    '=':'>':r                 -> TokenMAr : (lexer $ tail r)
    'e':'n':'d':r             -> TokenEnd : (lexer $ tail r)
    's':'e':'l':r             -> TokenarB : (lexer $ tail r)
    'u':'n':'l':'e':'s':'s':r -> TokenUnl : (lexer $ tail r)
    'r':'e':'p':'e':'a':'t':r -> TokenPer : (lexer $ tail r)
    '§':r                     -> TokenGrd : lexer r
    '|':r                     -> TokenraP : lexer r
    '+':r                     -> TokenBra : lexer r
    ':':r                     -> TokenSec : lexer r
    ';':r                     -> TokenqeS : lexer r
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
checkToken :: Token -> (RGG, Set Ptp) -> [RGG]
checkToken t (g,_) = case t of
                      TokenraP -> case g of
                                   Rap l -> l
                                   _ -> [g]
                      TokenqeS -> case g of
                                   Qes l -> l
                                   _ -> [g]
                      _        -> [g]


myErr :: String -> a
myErr err = error ("sggparser: ERROR - " ++ err)
}
