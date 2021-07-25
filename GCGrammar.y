--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--

-- A very basic grammar and parser for the textual editing of global
-- graphs. The grammar is a revised version of the one used in the
-- ICE16 paper with the extensions for reversibility-enabled graphs of
-- DAIS 18
--
--    G ::= (o)
--       |  P -> P : M
--       |  P => P, ..., P : M
--	 |  G | G
--       |  sel { Brc }
--       |  sel P { Brc }
--       |  branch { Brc }
--       |  branch P { Brc }
--       |  G ; G
--       |  * G @ P
--       |  repeat { G unless guard }
--       |  repeat P { G unless guard }
--       |  { G }
--       |  ( G )                        -- this is deprecated. For backward compatibility only
--
--    Brc   ::= G | G unless guard | B + B
--
--    guard ::= P % str | P % str, guard
--
-- where '(o)' has a special role: it marks a point where the selector
-- of a loop may exit the iteration. Guards are used only for the
-- reversible semantics and the string in them is supposed to be some
-- valid Erlang code. Likewise for the 'sel' construct, which
-- generalises the choice for the reversible semantics. Notice that
-- the 'sel' and the 'branch' constructs have the same semantics and
-- allow to specify the selector of the branch (to simplify the
-- realisation of projections on Erlang, the selector is mandatory for
-- REGs and optional otherwise). The clause 'unless guard' is optional
-- in branching and iteration.
--
-- The parser for the forward assumes the following equalities
--
--   sel P { Gn unless g1 + ... + Gn unless gn } = G1 + ... + Gn      for all guards g1, ..., gn 
--   repeat P {G unless g}                       = * G @ P            for all guards g
--
-- The binary operators _ | _, _ + _, and _ ; _ are given in ascending order of
-- precedence.
--
-- Note: strings are made of the following characters
--
--   0123456789
--   <=>\\^_`\167/$#&~,.
--   ABCDEFGHIJKLMNOPQRSTUVWXYZ
--   abcdefghijklmnopqrstuvwxyz
--
-- and must start with a letter when specifying the identity of a
-- participant.
--
-- Reserved characters not allowed in strings are:
--
--   @ . , ; : ( ) [ ] { } | + * ! ? - % §
--
-- Text enclosed by '[' and ']' is treated as multi-line comment and,
-- after '..', so is the rest of a line.
--
-- The parser generator is Haskell's 'Happy' and the parser
-- (GCParser.hs) is obtained by typing 'make parser'.
--
-- Basic syntactic checks are made during the parsing (e.g, (i) that
-- sender and receiver of interactions have to be different and (2)
-- that the participant controlling a loop is active in the
-- loop). More checks are planned together with improved error
-- handling.
--

{
module GCParser where
import SyntacticGlobalChoreographies
import ErlanGC
import Data.Set as S (empty, singleton, intersection, union, unions, difference, fromList, difference, toList, member, foldr, Set)
import Data.List as L
import qualified Data.Map as M (keys, empty, insert, union, Map)
import Misc
import CFSM
}

%name gcgrammar
%tokentype { Token }
%monad { Ptype } { thenPtype } { returnPtype }
%lexer { lexer } { TokenEof }
%error { parseError }

%token
  str	        { TokenStr $$   }
  '(o)'         { TokenEmp      }
  '->'	     	{ TokenArr      }
  '=>'	        { TokenMAr      }
  '|'	        { TokenPar      }
  '+'	        { TokenBra      }
  '%'	        { TokenGrd      }
  '*'	        { TokenSta      }
  ';'	        { TokenSeq      }
  '@'   	{ TokenUnt      }
  ':'	        { TokenSec      }
  '('	        { TokenBro      }
  ')'	        { TokenBrc      }
  ','	        { TokenCom      }
  '{'	        { TokenCurlyo   }
  '}'	        { TokenCurlyc   }
  'sel'         { TokenSel      }
  'branch'      { TokenSel      }
  'repeat'      { TokenRep      }
  'unless'      { TokenUnl      }
  '--hsl--'     { TokenEof      }

%right '|'
%right '+'
%right '%'
%right ';'
%right ','

%%

G :: { (GC, Set Ptp) }
  G : B      { $1 }
  | B '|' G  { (Par ((checkToken TokenPar $1)
                     ++ (checkToken TokenPar $3)),
                 S.union (snd $1) (snd $3))
             }


B :: { (GC, Set Ptp) }
B : S                               { $1 }
  | choiceop '{' Br '+' Bs '}'      { (Bra (S.fromList $
                                            (L.foldr (\g -> \l -> l ++ (checkToken TokenBra g))
                                             []
                                             (L.map fst ([$3] ++ $5))
                                            )
                                           ),
                                        ptpsBranches ([$3] ++ $5))
                                    }
  | choiceop str '{' Br '+' Bs '}'  { (Bra (S.fromList $
                                            (L.foldr (\g -> \l -> l ++ (checkToken TokenBra g))
                                             []
                                             (L.map fst ([$4] ++ $6))
                                            )
                                           ),
                                        ptpsBranches ([$4] ++ $6))
                                    }


choiceop : 'sel'     {}
         | 'branch'  {}


Bs :: { [((GC, Set Ptp), M.Map String String)] }
Bs : Br         { [$1] }
   | Br '+' Bs  { [$1] ++ $3 }


Br :: { ((GC, Set Ptp), M.Map String String) }
Br : S                 { ($1, M.empty) }
   | S 'unless' guard  { checkGuard $1 $3 }


S :: { (GC, Set Ptp) }
S : '(o)'                               { (Emp, S.empty) }
  | Blk                                 { $1 }
  | B ';' B                             { (Seq ((checkToken TokenSeq $1)
                                                 ++ (checkToken TokenSeq $3)),
                                            S.union (snd $1) (snd $3))
                                        }



Blk :: { (GC, Set Ptp) }
Blk : str '->' str ':' str              { case ((isPtp $1), (isPtp $3), ($1 == $3)) of
        				    (True, True, False)   -> ((Act ($1 , $3) $5), S.fromList [$1,$3])
        				    (False, False, _) -> myErr ("Bad names " ++ $1 ++ " and " ++ $3)
	        			    (False, True, True)    -> myErr ("Bad name " ++ $1 ++ " and sender and receiver must be different")
	        			    (False, True, False)    -> myErr ("Bad name " ++ $1)
	        			    (True, False, False)  -> myErr ("Bad name " ++ $3)
		        		    (_, _, True)  -> myErr ("Sender " ++ $1 ++ " cannot also be receiver in the same interaction")
                                        }
  | str '=>' ptps ':' str               { if (L.elem $1 $3)
                                          then myErr ($1 ++ " must NOT be one of the receivers " ++ (L.foldl (\x y -> if x == "" then y else x ++ ", " ++ y) "" $3))
                                          else case (isPtp $1, $3) of
                                                 (False, _)   -> myErr ("Bad name " ++ $1)
                                                 (True, [])   -> myErr ($1 ++ " cannot be empty") -- ($1 ++ " => " ++ "[]")
                                                 (True, s:[]) -> ((Act ($1 , s) $5), S.fromList([$1,s]))
                                                 _            -> (Par (L.map (\s -> (Act ($1 , s) $5)) $3),S.fromList($1:$3))
                                        }
  | '*' G '@' str                       {
      			        	  case ((isPtp $4), (S.member $4 (snd $2))) of
                                            (True, True)  -> (Rep (fst $2) $4 , S.union (S.singleton $4) (snd $2))
                                            (False, _)    -> myErr ("Bad name " ++ $4)
                                            (True, False) -> myErr ("Participant " ++ $4 ++ " is not among the loop's participants: " ++ (show $ toList $ snd $2))
                                        }
  | 'repeat' str '{' G '}'              {
              				  case ((isPtp $2), (S.member $2 (snd $4))) of
                                            (True, True)  -> (Rep (fst $4) $2 , S.union (S.singleton $2) (snd $4))
                                            (False, _)    -> myErr ("Bad name " ++ $2)
                                            (True, False) -> myErr ("Participant " ++ $2 ++ " is not among the loop's participants: " ++ (show $ toList $ snd $4))
                                        }
  | 'repeat' str '{' G 'unless' guard '}'    {
                                               case ((isPtp $2), (S.member $2 (snd $4))) of
                                                 (True, True)  -> (Rep (fst $4) $2 , S.union (S.singleton $2) (snd $4))
                                                 (False, _)    -> myErr ("Bad name " ++ $2)
                                                 (True, False) -> myErr ("Participant " ++ $2 ++ " is not among the loop's participants: " ++ (show $ toList $ snd $4))
                                             }
  | '(' G ')'                           { $2 }    -- this is for backward compatibility and it is deprecated
  | '{' G '}'                           { $2 }


guard :: { M.Map String String }
guard : str '%' str             { M.insert $1 $3 M.empty }
      | str '%' str ',' guard   { M.insert $1 $3 $5 }


ptps :: { [String] }
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
  | TokenEmp
  | TokenArr
  | TokenPar
  | TokenBra
  | TokenSel
  | TokenGrd
  | TokenSeq
  | TokenRep
  | TokenSta
  | TokenUnt
  | TokenSec
  | TokenBro
  | TokenBrc
  | TokenCom
  | TokenMAr
  | TokenUnl
  | TokenEof
  | TokenCurlyo
  | TokenCurlyc
        deriving (Show)

lexer :: (Token -> Ptype a) -> Ptype a
lexer cont s l c =
  case s of
    []                             -> (cont TokenEof) "" l c
    '(':'o':')':r                  -> cont TokenEmp r l (c+3)
    '.':'.':r                      -> (lexer cont) (dropWhile (\c->c/='\n') r) (l+1) 0
    ' ':r                          -> (lexer cont) r l (c+1)
    '\n':r                         -> (lexer cont) r (l+1) 0
    '\t':r                         -> (lexer cont) r l (c+1)
    '-':'>':r                      -> cont TokenArr r l (c+2)
    '=':'>':r                      -> cont TokenMAr r l (c+2)
    '|':r                          -> cont TokenPar r l (c+1)
    '+':r                          -> cont TokenBra r l (c+1)
    's':'e':'l':' ':r              -> cont TokenSel r l (c+4)
    's':'e':'l':'\n':r             -> cont TokenSel r (l+1) 0
    's':'e':'l':'\t':r             -> cont TokenSel r l (c+4)
    'b':'r':'a':'n':'c':'h':' ':r  -> cont TokenSel r l (c+7)
    'b':'r':'a':'n':'c':'h':'\n':r -> cont TokenSel r (l+1) 0
    'b':'r':'a':'n':'c':'h':'\t':r -> cont TokenSel r l (c+7)
    '*':r                          -> cont TokenSta r l (c+1)
    'r':'e':'p':'e':'a':'t':' ':r  -> cont TokenRep r l (c+8)
    'r':'e':'p':'e':'a':'t':'\n':r -> cont TokenRep r (l+1) 0
    'r':'e':'p':'e':'a':'t':'\t':r -> cont TokenRep r l (c+8)
    'u':'n':'l':'e':'s':'s':' ':r  -> cont TokenUnl r l (c+7)
    'u':'n':'l':'e':'s':'s':'\t':r -> cont TokenUnl r (l+1) 0
    'u':'n':'l':'e':'s':'s':'\r':r -> cont TokenUnl r l (c+7)
    '%':r                          -> cont TokenGrd r l (c+1)
    '@':r                          -> cont TokenUnt r l (c+1)
    ':':r                          -> cont TokenSec r l (c+1)
    ';':r                          -> cont TokenSeq r l (c+1)
    ',':r                          -> cont TokenCom r l (c+1)
    '(':r                          -> cont TokenBro r l (c+1)
    ')':r                          -> cont TokenBrc r l (c+1)
    '{':r                          -> cont TokenCurlyo r l (c+1)
    '}':r                          -> cont TokenCurlyc r l (c+1)
    '-':'-':'h':'s':'l':'-':'-':_  -> cont TokenEof [] l c
    '[':r ->
      let
        tmp = L.takeWhile (\c -> c /= ']') r
        r' = L.dropWhile (\c -> c /= ']') r
        l' = l + L.length (L.filter (\c -> c == '\n') tmp)
        c' = c + if l'==0 then (length tmp) else 0
      in
        if r' == ""
        then Er (synErr l c "multiline comment not closed")
        else lexer cont (tail r') l' c'
    _                              -> (cont (TokenStr (fst s'))) (snd s') l (c + (length s'))
        where s' = span isAlpha s

data ParseResult a =
  Ok a
  | Er String
  deriving (Show)

type Ptype a = String -> Int -> Int -> ParseResult a

parseError token =
  \ _ l c ->
    case token of
      TokenStr s  -> Er (synErr l c "string format violation")
      TokenEmp    -> Er (synErr l c "Unexpected (o)")
      TokenArr    -> Er (synErr l c "Unexpected \'->\'")
      TokenPar    -> Er (synErr l c "Unexpected gate \'|\'")
      TokenBra    -> Er (synErr l c "Unexpected branch gate")
      TokenSel    -> Er (synErr l c "Unexpected branch gate")
      TokenGrd    -> Er (synErr l c "Unexpected guard")
      TokenSeq    -> Er (synErr l c "Unexpected \';\'")
      TokenRep    -> Er (synErr l c "Unexpected loop")
      TokenSta    -> Er (synErr l c "Unexpected loop")
      TokenUnt    -> Er (synErr l c "Unexpected \'@\'")
      TokenSec    -> Er (synErr l c "Unexpected \':\'")
      TokenBro    -> Er (synErr l c "Unexpected \'(\'")
      TokenBrc    -> Er (synErr l c "Unexpected \')\'")
      TokenCom    -> Er (synErr l c "Unexpected \',\'")
      TokenMAr    -> Er (synErr l c "Unexpected =>")
      TokenUnl    -> Er (synErr l c "Unexpected \'unless\' clause")
      TokenCurlyo -> Er (synErr l c "Unexpected \'{\'")
      TokenCurlyc -> Er (synErr l c "Unexpected \'}\'")
      TokenEof    -> Er (synErr l c "Parse error; perhaps an unexpected trailing symbol")

thenPtype :: Ptype a -> (a -> Ptype b) -> Ptype b
m `thenPtype` k = \s l c ->
  case m s l c of
    Ok v -> k v s l c
    Er e -> Er e

returnPtype :: a -> Ptype a
returnPtype a = \s _ _ -> Ok a

failPtype :: String -> Ptype a
failPtype err = \_ _ _ -> Er err

-- GC specific functions

synErr :: Int -> Int -> String -> String
synErr l c err = "Syntax error at <" ++ (show l) ++ "," ++ (show $ c+1) ++ ">: " ++ err

myErr :: String -> a
myErr err = error ("gcparser: ERROR - " ++ err)

ptpsBranches :: [((GC, Set Ptp), ReversionGuard)] -> Set Ptp
ptpsBranches =
  \l -> L.foldr S.union S.empty (L.map (\x -> snd $ fst x) l)


checkGuard :: (GC, Set Ptp) -> ReversionGuard -> ((GC, Set Ptp), ReversionGuard)
checkGuard g m =
  let tmp = [ x | x <- M.keys m, not (S.member x (snd g)) ]
  in
    if L.null tmp
    then (g, m)
    else myErr ("Unknown participant(s): " ++ (show tmp))

-- checkToken 'flattens', parallel, branching, and sequential composition
checkToken :: Token -> (GC, Set Ptp) -> [GC]
checkToken t (g,_) =
    case t of
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
ggsptp :: Set Ptp -> GC -> Set Ptp
ggsptp ps g = case g of
               Emp         -> ps
               Act (s,r) _ -> S.union ps (S.fromList [s,r])
               Par gs      -> S.union ps (S.unions (L.map (ggsptp S.empty) gs))
               Bra gs      -> S.union ps (S.unions (L.map (ggsptp S.empty) (S.toList gs)))
               Seq gs      -> S.union ps (S.unions (L.map (ggsptp S.empty) gs))
               Rep g' p    -> S.union ps (ggsptp (S.singleton p) g')

    
}
