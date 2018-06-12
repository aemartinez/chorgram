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
import Data.Set as S (empty, null, intersection, union, difference, fromList, member, Set)
import qualified Data.Map as M (keys, empty, insert, union, Map)
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
  '(o)'	        { TokenPme    }
  '%'	        { TokenGrd    }
  '->'	     	{ TokenArr    }
  '=>'	        { TokenMAr    }
  '|'	        { TokenraP    }
  '+'	        { TokenBra    }
  ';'	        { TokenqeS    }
  '@'   	{ TokenUnt    }
  ':'	        { TokenSec    }
  ','	        { TokenCom    }
  '('	        { TokenBro    }
  ')'	        { TokenBrc    }
  '{'	        { TokenCurlyo }
  '}'	        { TokenCurlyc }
  'sel'         { TokenarB    }
  'repeat'      { TokenPer    }
  '*'	        { TokenPer    }
  'unless'      { TokenUnl    }


%right '|'
%right '+'
%right ';'
%right '%'
%right ','

%%

G : str '->' str ':' str        { case ((isPtp $1), (isPtp $3), not($1 == $3)) of
				    (True, True, True)   -> ((Tca ($1 , $3) $5), S.fromList [$1,$3])
				    (True, False, True)  -> myErr ("Bad name " ++ $3)
				    (True, True, False)  -> myErr ("A sender " ++ $3 ++ " cannot be also the receiver in an interaction")
				    (True, False, False) -> myErr ("Now, this is odd... A sender " ++ $1 ++ " and " ++ $3 ++ " are equal but different")
				    (False, True, True)  -> myErr ("Now, this is odd... A sender " ++ $1 ++ " and " ++ $3 ++ " are equal but different")
				    (False, False, True) -> myErr ("Bad names " ++ $1 ++ " and " ++ $3)
				    (False, _, False)    -> myErr ("Bad name " ++ $1 ++ " and sender and receiver must be different")
                                }

  | str '=>' ptps ':' str       { case ((isPtp $1), not(L.elem $1 $3)) of
                                     (True, True)   -> case $3 of
                                                         []   -> myErr ($1 ++ " cannot be empty") -- ($1 ++ " => " ++ "[]")
                                                         s:[] -> ((Tca ($1 , s) $5), S.fromList([$1,s]))
                                                         _    -> ((Rap (L.map (\s -> (Tca ($1, s) $5)) $3), S.fromList($1:$3)))
                                     (True, False)  -> myErr ($1 ++ " must be in " ++ (show $3))
                                     (False, _)     -> myErr ("Bad name " ++ $1)
                                }

  | G '|' G  	     		{ let ptps = (S.intersection (snd $1) (snd $3)) in
                                  if S.null ptps
                                  then case (not (emptyG $ fst $1), not (emptyG $ fst $3)) of
                                         (True,  True)  -> (Rap ((checkToken TokenraP (fst $1)) ++ (checkToken TokenraP (fst $3))),
                                                            S.union (snd $1) (snd $3))
                                         (True,  False) -> $1
                                         (False, True)  -> $3
                                         (False, False) -> (Pme, S.empty)
                                  else myErr("Non disjoint threads " ++ (show ptps))
                                }

  | 'sel' str '{' branch '}'	{ let p = S.fromList $ L.concat (L.map (\(_, guard) -> M.keys guard) $4) in
                                    case isPtp $2 of
                                       True  -> let branches = L.map (\((g, _), guard) -> (g, guard)) $4 in ((Arb $2 branches), p)
                                       False -> myErr ("Bad name " ++ $2)
                                }

  | G ';' G  	     		{ case (not (emptyG $ fst $1), not (emptyG $ fst $3)) of
                                    (True, True)   -> (Qes ((checkToken TokenqeS (fst $1)) ++ (checkToken TokenqeS (fst $3))), S.union (snd $1) (snd $3))
                                    (True, False)  -> $1
                                    (False, True)  -> $3
                                    (False, False) -> (Pme, S.empty)
                                }

  | 'repeat' str '{' G
                     'unless'
                     guard
                 '}'            { case (isPtp $2, S.member $2 (snd $4)) of
                                    (True,  True)  -> let cg = checkGuard $4 $6 in (Per $2 (fst $ fst cg) (snd cg), snd $4)
                                    (True,  False) -> myErr("Participant " ++ $2 ++ " must be in the body of the loop")
                                    (False, True)  -> myErr("Bad name " ++ $2)
                                    (False, False) -> myErr("Bad name " ++ $2 ++ " (and a selector must be in the body)")
                                }

  | '(' G ')'			{ ( $2 ) }

  | '{' G '}'			{ ( $2 ) }

  | '(o)'                       { (Pme, S.empty) }



guard : str '%' str             { M.empty }

      | str '%' str ',' guard   { M.insert $1 $3 $5 }



branch : G                      { [ ($1, M.empty) ] }

       | G 'unless' guard       { [ checkGuard $1 $3 ] }

       | branch '+' branch      { $1 ++ $3 }



ptps : str                      { if (isPtp $1) then [$1] else myErr ("Bad name " ++ $1) }

     | str ',' ptps             { if (isPtp $1)
                                  then (case $3 of
                                          [] ->  [$1]
                                          (s:l) -> ($1:s:l))
                                  else myErr ("Bad name " ++ $1)
                                }


{
data Token =
  TokenStr String
  | TokenPtps [Ptp]
  | TokenPme
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
  | TokenUnl
  | TokenErr String
  deriving Show


-- lexer :: String -> [Token]
-- lexer :: (Token -> Err a) -> Err a
lexer s = case s of
    []                        -> []
    '(':'o':')':r             -> TokenPme : lexer r
    '[':r                     -> lexer $ tail (L.dropWhile (\c->c/=']') r)   -- multi-line comment
    '.':'.':r                 -> lexer $ tail (L.dropWhile (\c->c/='\n') r)  -- single-line comment
    ' ':r                     -> lexer r
    '\n':r                    -> lexer r
    '\t':r                    -> lexer r
    '-':'>':r                 -> TokenArr : (lexer $ tail r)
    '=':'>':r                 -> TokenMAr : (lexer $ tail r)
    's':'e':'l':r             -> TokenarB : (lexer $ tail r)
    'u':'n':'l':'e':'s':'s':r -> TokenUnl : (lexer $ tail r)
    'r':'e':'p':'e':'a':'t':r -> TokenPer : (lexer $ tail r)
    '@':r                     -> TokenUnt : lexer r
    '%':r                     -> TokenGrd : lexer r
    '|':r                     -> TokenraP : lexer r
    '+':r                     -> TokenBra : lexer r
    ':':r                     -> TokenSec : lexer r
    ';':r                     -> TokenqeS : lexer r
    ',':r                     -> TokenCom : lexer r
    '(':r                     -> TokenBro : lexer r
    ')':r                     -> TokenBrc : lexer r
    '(':'o':')':r             -> TokenPme : lexer r
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

checkGuard :: (RGG, Set Ptp) -> ReversionGuard -> ((RGG, Set Ptp), ReversionGuard)
checkGuard g m = let tmp = [ x | x <- M.keys m, not (S.member x (snd g)) ] in
                 if L.null tmp
                 then (g, m)
                 else myErr ("Unknown participant(s): " ++ (show tmp))

-- type LineNumber = Int

-- data ParseResult a = Ok a | Failed String
-- type P a = String -> ParseResult a


-- getLineNo :: P LineNumber
-- getLineNo = \s l -> Ok l

--
-- Plagiarism done
--
-- checkToken 'flattens', parallel and sequential composition
checkToken :: Token -> RGG -> [RGG]
checkToken t g = case t of
                   TokenraP -> case g of
                                 Rap l -> l
                                 _ -> [g]
                   TokenqeS -> case g of
                                 Qes l -> l
                                 _ -> [g]
                   _        -> [g]

emptyG :: RGG -> Bool
emptyG g = case g of
             Pme -> True
             _   -> False
  
myErr :: String -> a
myErr err = error ("sggparser: ERROR - " ++ err)
}
