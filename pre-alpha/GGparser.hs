{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module GGparser where
import SyntacticGlobalGraphs
import ErlanGG
import Data.Set as S (empty, singleton, intersection, union, unions, difference, fromList, difference, toList, member, foldr, Set)
import Data.List as L
import qualified Data.Map as M (keys, empty, insert, union, Map)
import Misc
import CFSM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ((GG, Set Ptp))
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 ([((GG, Set Ptp), M.Map String String)])
	| HappyAbsSyn8 (((GG, Set Ptp), M.Map String String))
	| HappyAbsSyn11 (M.Map String String)
	| HappyAbsSyn12 ([String])

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,138) ([12288,29832,24576,59664,0,64,32768,8192,0,0,0,0,0,48,0,0,0,34864,116,4192,233,8384,466,0,0,0,0,512,0,0,0,32768,8,12288,29832,24576,59664,0,4096,0,16384,0,4096,0,1024,0,4,0,8,0,0,4,4192,233,16384,0,4096,0,0,2048,1536,3729,0,16,0,256,0,64,8192,0,0,0,0,0,0,18563,7,512,0,0,0,0,68,0,0,32,0,64,0,128,0,8192,0,512,0,3072,7458,0,1024,0,2,0,0,0,16,32768,42049,3,0,0,0,0,0,0,0,0,16,0,0,16,0,32,128,0,33536,1864,0,0,0,0,0,256,0,0,0,0,16384,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_gggrammar","G","B","choiceop","Bs","Br","S","Blk","guard","ptps","str","'(o)'","'->'","'=>'","'|'","'+'","'%'","'*'","';'","'@'","':'","'('","')'","','","'{'","'}'","'sel'","'branch'","'repeat'","'unless'","%eof"]
        bit_start = st * 33
        bit_end = (st + 1) * 33
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..32]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,68) ([1,1,8,6,0,0,2,0,1,1,1,0,0,24,5,-1,1,1,17,23,20,30,45,46,38,1,51,48,-8,1,73,47,79,96,0,0,1,92,0,80,0,101,102,103,99,105,1,91,104,0,106,1,0,0,0,0,107,95,98,108,1,0,0,109,0,0,111,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,68) ([22,9,0,0,0,0,0,0,29,36,43,0,0,0,0,0,50,93,0,0,0,0,0,110,0,81,0,0,0,87,0,0,0,0,0,0,57,0,0,0,0,0,112,0,0,114,63,0,0,0,0,69,0,0,0,0,115,0,0,0,75,0,0,0,0,0,117,0,0
	])

happyAdjustOffset :: Int -> Int
happyAdjustOffset = id

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,68) ([0,0,0,0,-4,-14,0,-13,0,0,0,-7,-8,0,0,-2,0,0,0,0,0,0,0,0,0,0,0,0,-11,0,0,-25,0,0,-21,-22,0,-15,-3,0,-18,0,0,0,0,0,0,0,-9,-12,0,0,-17,-26,-16,-19,0,0,0,0,0,-5,-10,-23,-6,-20,0,-24
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,138) ([-1,9,1,2,5,3,4,1,9,8,1,2,20,12,5,6,15,9,17,18,19,15,0,1,2,1,21,5,6,0,1,2,15,13,5,6,0,1,2,16,10,5,6,0,1,2,1,1,5,6,0,1,2,15,6,5,6,0,1,2,9,14,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,11,4,5,6,1,2,11,4,5,6,1,2,16,1,5,6,20,9,1,1,1,6,1,16,1,1,6,16,1,7,16,-1,-1,-1,8,-1,8,7,7,14,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,138) ([0,-4,7,8,17,23,24,25,18,9,2,3,46,10,4,5,11,18,12,13,14,26,14,15,3,19,-1,4,5,21,15,3,37,35,4,5,20,15,3,36,34,4,5,19,15,3,33,32,4,5,38,15,3,30,47,4,5,39,15,3,18,43,4,5,26,3,47,48,28,5,26,3,58,48,28,5,26,3,62,48,28,5,26,3,44,27,28,5,26,3,42,44,28,5,37,3,56,41,4,5,57,18,55,32,53,52,51,62,51,64,61,66,51,60,65,0,0,0,30,0,53,49,57,67,67,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 25) [
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
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25)
	]

happy_n_terms = 22 :: Int
happy_n_nonterms = 9 :: Int

happyReduce_1 = happySpecReduce_1  0 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  0 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Par ((checkToken TokenPar happy_var_1)
                                                ++ (checkToken TokenPar happy_var_3)),
                                            S.union (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  1 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 1 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((Bra (S.fromList $
                                                 (L.foldr (\g -> \l -> l ++ (checkToken TokenBra g))
                                                   []
                                                   (L.map fst ([happy_var_3] ++ happy_var_5))
                                                 )
                                               ),
                                            ptpsBranches ([happy_var_3] ++ happy_var_5))
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 7 1 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((Bra (S.fromList $
                                                 (L.foldr (\g -> \l -> l ++ (checkToken TokenBra g))
                                                   []
                                                   (L.map fst ([happy_var_4] ++ happy_var_6))
                                                 )
                                               ),
                                            ptpsBranches ([happy_var_4] ++ happy_var_6))
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  2 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn6
		 (
	)

happyReduce_7 = happySpecReduce_1  2 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn6
		 (
	)

happyReduce_8 = happySpecReduce_1  3 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([ happy_var_1 ]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  3 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ((happy_var_1, M.empty)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (checkGuard happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn4
		 ((Emp, S.empty)
	)

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Seq ((checkToken TokenSeq happy_var_1)
                                                 ++ (checkToken TokenSeq happy_var_3)),
                                            S.union (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 5 6 happyReduction_15
happyReduction_15 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), (isPtp happy_var_3), not(happy_var_1 == happy_var_3)) of
        				    (True, True, True)   -> ((Act (happy_var_1 , happy_var_3) happy_var_5), S.fromList [happy_var_1,happy_var_3])
	        			    (True, False, True)  -> myErr ("Bad name " ++ happy_var_3)
		        		    (True, True, False)  -> myErr ("A sender " ++ happy_var_3 ++ " cannot be also the receiver in an interaction")
			        	    (_, False, False)    -> myErr ("Whaaat??? Sender " ++ happy_var_1 ++ " and receiver " ++ happy_var_3 ++ " are equal AND different!!!")
				            (_, True, True)      -> myErr ("Whaaat??? Sender " ++ happy_var_1 ++ " and receiver " ++ happy_var_3 ++ " are equal AND different!!!")
        				    (False, False, True) -> myErr ("Bad names " ++ happy_var_1 ++ " and " ++ happy_var_3)
	        			    (False, _, False)    -> myErr ("Bad name " ++ happy_var_1 ++ " and sender and receiver must be different")
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 5 6 happyReduction_16
happyReduction_16 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), not(L.elem happy_var_1 happy_var_3)) of
                                            (True,  True)  -> case happy_var_3 of
                                                                []   -> myErr (happy_var_1 ++ " cannot be empty") -- (happy_var_1 ++ " => " ++ "[]")
                                                                s:[] -> ((Act (happy_var_1 , s) happy_var_5), S.fromList([happy_var_1,s]))
                                                                _    -> (Par (L.map (\s -> (Act (happy_var_1 , s) happy_var_5)) happy_var_3),S.fromList(happy_var_1:happy_var_3))
                                            (True,  False) -> myErr (happy_var_1 ++ " must be in " ++ (show happy_var_3))
                                            (False, _)     -> myErr ("Bad name " ++ happy_var_1)
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 6 happyReduction_17
happyReduction_17 ((HappyTerminal (TokenStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_4), (S.member happy_var_4 (snd happy_var_2))) of
                                            (True, True)  -> (Rep (fst happy_var_2) happy_var_4 , S.union (S.singleton happy_var_4) (snd happy_var_2))
                                            (False, _)    -> myErr ("Bad name " ++ happy_var_4)
                                            (True, False) -> myErr ("Participant " ++ happy_var_4 ++ " is not among the loop's participants: " ++ (show $ toList $ snd happy_var_2))
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 6 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_2), (S.member happy_var_2 (snd happy_var_4))) of
                                            (True, True)  -> (Rep (fst happy_var_4) happy_var_2 , S.union (S.singleton happy_var_2) (snd happy_var_4))
                                            (False, _)    -> myErr ("Bad name " ++ happy_var_2)
                                            (True, False) -> myErr ("Participant " ++ happy_var_2 ++ " is not among the loop's participants: " ++ (show $ toList $ snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 7 6 happyReduction_19
happyReduction_19 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_2), (S.member happy_var_2 (snd happy_var_4))) of
                                                 (True, True)  -> (Rep (fst happy_var_4) happy_var_2 , S.union (S.singleton happy_var_2) (snd happy_var_4))
                                                 (False, _)    -> myErr ("Bad name " ++ happy_var_2)
                                                 (True, False) -> myErr ("Participant " ++ happy_var_2 ++ " is not among the loop's participants: " ++ (show $ toList $ snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  7 happyReduction_22
happyReduction_22 (HappyTerminal (TokenStr happy_var_3))
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn11
		 (M.insert happy_var_1 happy_var_3 M.empty
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 7 happyReduction_23
happyReduction_23 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (M.insert happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn12
		 (if (isPtp happy_var_1) then [happy_var_1] else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  8 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn12
		 (if (isPtp happy_var_1)
                                  then (case happy_var_3 of
                                        [] ->  [happy_var_1]
                                        (s:l) -> (happy_var_1:s:l))
                                  else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 21 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenStr happy_dollar_dollar -> cont 1;
	TokenEmp -> cont 2;
	TokenArr -> cont 3;
	TokenMAr -> cont 4;
	TokenPar -> cont 5;
	TokenBra -> cont 6;
	TokenGrd -> cont 7;
	TokenSta -> cont 8;
	TokenSeq -> cont 9;
	TokenUnt -> cont 10;
	TokenSec -> cont 11;
	TokenBro -> cont 12;
	TokenBrc -> cont 13;
	TokenCom -> cont 14;
	TokenCurlyo -> cont 15;
	TokenCurlyc -> cont 16;
	TokenSel -> cont 17;
	TokenSel -> cont 18;
	TokenRep -> cont 19;
	TokenUnl -> cont 20;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 21 tk tks = happyError' (tks, explist)
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
gggrammar tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Token =
  TokenStr String
  | TokenPtps [Ptp]
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
  | TokenErr String
  | TokenCurlyo
  | TokenCurlyc
        deriving (Show)


lexer s = case s of
    []                             -> []
    '(':'o':')':r                  -> TokenEmp : lexer r
    '[':r                          -> lexer $ mytail (L.dropWhile (\c->c/=']') r)
    '.':'.':r                      -> lexer $ mytail (L.dropWhile (\c->c/='\n') r)
    ' ':r                          -> lexer r
    '\n':r                         -> lexer r
    '\t':r                         -> lexer r
    '-':'>':r                      -> TokenArr : (lexer r)
    '=':'>':r                      -> TokenMAr : (lexer r)
    '|':r                          -> TokenPar : lexer r
    '+':r                          -> TokenBra : lexer r
    's':'e':'l':' ':r              -> TokenSel : (lexer r)
    's':'e':'l':'\n':r             -> TokenSel : (lexer r)
    's':'e':'l':'\t':r             -> TokenSel : (lexer r)
    'b':'r':'a':'n':'c':'h':' ':r  -> TokenSel : (lexer r)
    'b':'r':'a':'n':'c':'h':'\n':r -> TokenSel : (lexer r)
    'b':'r':'a':'n':'c':'h':'\t':r -> TokenSel : (lexer r)
    '*':r                          -> TokenSta : lexer r
    'r':'e':'p':'e':'a':'t':' ':r  -> TokenRep : (lexer r)
    'r':'e':'p':'e':'a':'t':'\n':r -> TokenRep : (lexer r)
    'r':'e':'p':'e':'a':'t':'\t':r -> TokenRep : (lexer r)
    'u':'n':'l':'e':'s':'s':' ':r  -> TokenUnl : (lexer r)
    'u':'n':'l':'e':'s':'s':'\t':r -> TokenUnl : (lexer r)
    'u':'n':'l':'e':'s':'s':'\r':r -> TokenUnl : (lexer r)
    '%':r                          -> TokenGrd : lexer r
    '@':r                          -> TokenUnt : lexer r
    ':':r                          -> TokenSec : lexer r
    ';':r                          -> TokenSeq : lexer r
    ',':r                          -> TokenCom : lexer r
    '(':r                          -> TokenBro : lexer r
    ')':r                          -> TokenBrc : lexer r
    '{':r                          -> TokenCurlyo : lexer r
    '}':r                          -> TokenCurlyc : lexer r
    _                              -> TokenStr (fst s') : (lexer $ snd s')
        where s' = span isAlpha s

mytail :: [t] -> [t]
mytail l = if L.null l
           then l
           else tail l

parseError :: [Token] -> a
parseError err = case err of
                    TokenErr s:_ -> myErr $ show s
                    _            -> myErr (show err)


ptpsBranches :: [((GG, Set Ptp), ReversionGuard)] -> Set Ptp
ptpsBranches = \l -> L.foldr S.union S.empty (L.map (\x -> snd $ fst x) l)


checkGuard :: (GG, Set Ptp) -> ReversionGuard -> ((GG, Set Ptp), ReversionGuard)
checkGuard g m = let tmp = [ x | x <- M.keys m, not (S.member x (snd g)) ] in
                 if L.null tmp
                 then (g, m)
                 else myErr ("Unknown participant(s): " ++ (show tmp))

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
