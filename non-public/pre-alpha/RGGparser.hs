{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module RGGparser where
import SyntacticGlobalGraphs
import ErlanGG
import Data.List as L
import Data.Set as S (empty, null, intersection, union, difference, toList, fromList, member, Set)
import qualified Data.Map as M (keys, empty, insert, union, Map)
import Misc
import CFSM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,85) ([384,212,4,0,1,40960,0,192,0,0,1536,848,32816,32794,0,1024,0,0,4,8192,0,517,10240,4,2,4096,0,384,212,40972,6,16,40960,0,4096,0,256,0,4,0,0,0,3072,1696,96,53,16544,4,8197,4096,16,2,4096,0,128,0,0,0,0,0,0,16408,13,0,512,0,0,32768,0,0,256,128,0,0,0,2,64,0,0,0,64,128,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_rgggrammar","G","guard","branch","ptps","str","'(o)'","'%'","'->'","'=>'","'|'","'+'","';'","'@'","':'","','","'('","')'","'{'","'}'","'sel'","'repeat'","'*'","'unless'","%eof"]
        bit_start = st * 27
        bit_end = (st + 1) * 27
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..26]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,50) ([1,15,-3,-1,31,0,1,1,22,36,17,24,18,21,38,39,1,1,33,-2,32,34,37,0,0,1,1,6,3,13,42,43,45,0,0,0,1,0,47,0,47,35,46,0,44,51,0,48,52,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,50) ([54,0,0,0,0,0,55,56,0,0,0,0,0,0,0,57,58,61,0,0,0,0,0,0,0,8,62,0,0,0,0,60,0,0,0,0,30,0,63,0,64,0,0,0,0,0,0,0,65,0,0
	])

happyAdjustOffset :: Int -> Int
happyAdjustOffset = id

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,50) ([0,0,0,0,0,-11,0,0,0,0,0,0,0,0,0,0,0,0,-6,-4,0,-17,0,-9,-10,0,0,0,-14,0,0,0,0,-3,-18,-2,0,-5,0,-8,0,0,0,-15,-16,0,-7,-12,0,-13
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,85) ([-1,4,1,2,6,6,8,8,0,6,2,8,6,12,8,14,1,16,17,20,7,15,19,1,6,19,8,6,15,8,0,14,2,15,13,4,5,1,14,1,1,8,10,1,1,11,1,10,1,3,15,7,1,1,0,0,0,-1,0,11,3,0,0,3,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,85) ([0,15,5,6,17,17,18,18,28,17,29,18,17,7,18,8,3,9,10,-1,37,40,39,12,17,41,18,17,38,18,28,27,44,25,24,15,16,11,26,23,22,18,33,36,22,32,34,31,43,46,47,37,48,43,3,13,12,0,19,49,20,18,27,34,43,41,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 17) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17)
	]

happy_n_terms = 21 :: Int
happy_n_nonterms = 4 :: Int

happyReduce_1 = happyReduce 5 0 happyReduction_1
happyReduction_1 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), (isPtp happy_var_3), not(happy_var_1 == happy_var_3)) of
				    (True,  True,  True)  -> ((Tca (happy_var_1 , happy_var_3) happy_var_5), S.fromList [happy_var_1,happy_var_3])
				    (True,  False, True)  -> myErr ("Bad name " ++ happy_var_3)
				    (True,  True,  False) -> myErr ("A sender " ++ happy_var_3 ++ " cannot be also the receiver in an interaction")
				    (True,  False, False) -> myErr ("Now, this is odd... A sender " ++ happy_var_1 ++ " and " ++ happy_var_3 ++ " are equal but different")
				    (False, True,  True)  -> myErr ("Now, this is odd... A sender " ++ happy_var_1 ++ " and " ++ happy_var_3 ++ " are equal but different")
				    (False, False, True)  -> myErr ("Bad names " ++ happy_var_1 ++ " and " ++ happy_var_3)
				    (False, _,     False) -> myErr ("Bad name " ++ happy_var_1 ++ " and sender and receiver must be different")
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 5 0 happyReduction_2
happyReduction_2 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), not(L.elem happy_var_1 happy_var_3)) of
                                     (True,  True)   -> case happy_var_3 of
                                                         []   -> myErr (happy_var_1 ++ " cannot be empty") -- (happy_var_1 ++ " => " ++ "[]")
                                                         s:[] -> ((Tca (happy_var_1 , s) happy_var_5), S.fromList([happy_var_1,s]))
                                                         _    -> ((Rap (L.map (\s -> (Tca (happy_var_1, s) happy_var_5)) happy_var_3), S.fromList(happy_var_1:happy_var_3)))
                                     (True,  False)  -> myErr (happy_var_1 ++ " must be in " ++ (show happy_var_3))
                                     (False, _)      -> myErr ("Bad name " ++ happy_var_1)
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  0 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (let ptps = (S.intersection (snd happy_var_1) (snd happy_var_3)) in
                                  if True --S.null ptps
                                  then case (not (emptyG $ fst happy_var_1), not (emptyG $ fst happy_var_3)) of
                                         (True,  True)  -> (Rap ((checkToken TokenraP (fst happy_var_1)) ++ (checkToken TokenraP (fst happy_var_3))), S.union (snd happy_var_1) (snd happy_var_3))
                                         (True,  False) -> happy_var_1
                                         (False, True)  -> happy_var_3
                                         (False, False) -> (Pme, S.empty)
                                  else myErr("Non disjoint threads " ++ (show ptps))
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 0 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let p = S.fromList $ L.concat (L.map (\((_, ptps), guard) -> (S.toList ptps) ++ M.keys guard) happy_var_4) in
                                    case isPtp happy_var_2 of
                                       True  -> let branches = L.map (\((g, _), guard) -> (g, guard)) happy_var_4 in ((Arb happy_var_2 branches), p)
--TODO: FIX THIS: branches don't compute the list of participants correctly
                                       False -> myErr ("Bad name " ++ happy_var_2)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  0 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (case (not (emptyG $ fst happy_var_1), not (emptyG $ fst happy_var_3)) of
                                    (True,  True)  -> (Qes ((checkToken TokenqeS (fst happy_var_1)) ++ (checkToken TokenqeS (fst happy_var_3))), S.union (snd happy_var_1) (snd happy_var_3))
                                    (True,  False) -> happy_var_1
                                    (False, True)  -> happy_var_3
                                    (False, False) -> (Pme, S.empty)
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 7 0 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case (isPtp happy_var_2, S.member happy_var_2 (snd happy_var_4)) of
                                    (True,  True)  -> let cg = checkGuard happy_var_4 happy_var_6 in (Per happy_var_2 (fst $ fst cg) (snd cg), snd happy_var_4)
                                    (False, _) -> myErr("Bad name " ++ happy_var_2 ++ " (and a selector must be in the body)")
                                    (True, False) -> myErr ("Participant " ++ happy_var_2 ++ " is among the loop's participants: " ++ (show $ toList $ snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 0 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case (isPtp happy_var_2, S.member happy_var_2 (snd happy_var_4)) of
                                    (True,  True)  -> let cg = M.empty in (Per happy_var_2 (fst happy_var_4) cg, snd happy_var_4)
                                    (False, _)  -> myErr("Bad name " ++ happy_var_2)
                                    (True, False) -> myErr ("Participant " ++ happy_var_2 ++ " is among the loop's participants: " ++ (show $ toList $ snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  0 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (( happy_var_2 )
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  0 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (( happy_var_2 )
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  0 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn4
		 ((Pme, S.empty)
	)

happyReduce_11 = happySpecReduce_3  1 happyReduction_11
happyReduction_11 _
	_
	_
	 =  HappyAbsSyn5
		 (M.empty
	)

happyReduce_12 = happyReduce 5 1 happyReduction_12
happyReduction_12 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (M.insert happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  2 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([ (happy_var_1, M.empty) ]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  2 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([ checkGuard happy_var_1 happy_var_3 ]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  2 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  3 happyReduction_16
happyReduction_16 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn7
		 (if (isPtp happy_var_1) then [happy_var_1] else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  3 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn7
		 (if (isPtp happy_var_1)
                                  then (case happy_var_3 of
                                          [] ->  [happy_var_1]
                                          (s:l) -> (happy_var_1:s:l))
                                  else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 20 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenStr happy_dollar_dollar -> cont 1;
	TokenPme -> cont 2;
	TokenGrd -> cont 3;
	TokenArr -> cont 4;
	TokenMAr -> cont 5;
	TokenraP -> cont 6;
	TokenBra -> cont 7;
	TokenqeS -> cont 8;
	TokenUnt -> cont 9;
	TokenSec -> cont 10;
	TokenCom -> cont 11;
	TokenBro -> cont 12;
	TokenBrc -> cont 13;
	TokenCurlyo -> cont 14;
	TokenCurlyc -> cont 15;
	TokenarB -> cont 16;
	TokenPer -> cont 17;
	TokenPer -> cont 18;
	TokenUnl -> cont 19;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 20 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
rgggrammar tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
-- Starting to plagiarise from Happy's user manual (E has been renamed
-- with Err beause it clashed with the type of events)
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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc977c_0/ghc_2.h" #-}




























































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (0), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                (0)           -> {- nothing -}
                                     happyFail (happyExpListPerState ((st) :: Int)) i tk st
                (-1)          -> {- nothing -}
                                     happyAccept i tk st
                n | (n < ((0) :: Int)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((negate ((n + ((1) :: Int)))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n - ((1) :: Int))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off +  i)
         check  = if (off_i >= ((0) :: Int))
                  then (indexShortOffAddr happyCheck off_i ==  i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st



{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off +  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off +  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (0) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (0) tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction (0) tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction (0) tk action sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
