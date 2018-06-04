{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module RGGparser where
import SyntacticGlobalGraphs
import Data.List as L
import Data.Set as S
import Misc
import CFSM
import qualified Data.Array as Happy_Data_Array
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,44) ([1,59,56,-4,25,1,57,53,52,50,17,51,48,1,1,49,-1,47,46,43,0,1,1,13,6,45,44,42,0,0,0,41,41,27,24,38,1,0,0,0,2,40,23,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,44) ([39,0,0,0,0,37,0,0,0,0,0,0,36,35,34,0,0,0,0,0,0,33,32,0,0,0,29,0,0,0,0,20,16,0,11,0,8,0,0,0,0,4,0,0,0
	])

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,44) ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-6,-4,0,-11,0,-8,0,0,0,0,0,0,0,-3,-12,-2,0,0,0,0,0,0,-10,-9,-7,0,0,0,-5
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,77) ([-1,5,1,7,5,1,7,5,0,7,9,5,1,7,18,14,15,1,5,17,7,1,5,17,7,1,2,10,3,4,17,2,0,0,0,0,13,0,2,0,13,1,1,1,6,1,1,-1,-1,1,-1,8,1,-1,1,8,7,11,1,3,1,-1,12,-1,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,77) ([0,14,5,15,14,42,15,14,40,15,6,14,37,15,-1,7,8,33,14,42,15,35,14,32,15,35,39,21,12,13,33,29,23,24,15,16,44,10,17,3,40,35,35,29,37,19,31,0,0,19,0,26,20,0,9,28,15,27,10,12,3,0,22,0,23,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 11) [
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
	(11 , happyReduce_11)
	]

happy_n_terms = 19 :: Int
happy_n_nonterms = 3 :: Int

happyReduce_1 = happyReduce 5 0 happyReduction_1
happyReduction_1 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), (isPtp happy_var_3), not(happy_var_1 == happy_var_3)) of
				    (True, True, True)   -> ((Tca (happy_var_1 , happy_var_3) happy_var_5), S.fromList [happy_var_1,happy_var_3])
				    (True, False, True)  -> myErr ("Bad name " ++ happy_var_3)
				    (True, True, False)  -> myErr ("A sender " ++ happy_var_3 ++ " cannot be also the receiver in an interaction")
				    (True, False, False) -> myErr ("Now, this is odd... A sender " ++ happy_var_1 ++ " and " ++ happy_var_3 ++ " are equal but different")
				    (False, True, True)  -> myErr ("Now, this is odd... A sender " ++ happy_var_1 ++ " and " ++ happy_var_3 ++ " are equal but different")
				    (False, False, True) -> myErr ("Bad names " ++ happy_var_1 ++ " and " ++ happy_var_3)
				    (False, _, False)    -> myErr ("Bad name " ++ happy_var_1 ++ " and sender and receiver must be different")
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 5 0 happyReduction_2
happyReduction_2 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), not(L.elem happy_var_1 happy_var_3)) of
                                     (True, True)   -> case happy_var_3 of
                                                    []   -> myErr (happy_var_1 ++ " cannot be empty") -- (happy_var_1 ++ " => " ++ "[]")
                                                    s:[] -> ((Tca (happy_var_1 , s) happy_var_5), S.fromList([happy_var_1,s]))
                                                    _    -> (Rap (L.map (\s -> (Tca (happy_var_1 , s) happy_var_5)) happy_var_3),S.fromList(happy_var_1:happy_var_3))
                                     (True, False)  -> myErr (happy_var_1 ++ " must be in " ++ (show happy_var_3))
                                     (False, _)     -> myErr ("Bad name " ++ happy_var_1)
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  0 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Rap ((checkToken TokenraP happy_var_1) ++ (checkToken TokenraP happy_var_3)), S.union (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 11 0 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case (isPtp happy_var_2, (S.member happy_var_2 (S.union (snd happy_var_4) (snd happy_var_8)))) of
                                                            (True, True) -> (Arb happy_var_2 (fst happy_var_4,happy_var_6,fst happy_var_8,happy_var_10), S.union (snd happy_var_4) (snd happy_var_8))
                                                            (False,_)    -> myErr ("Bad name " ++ happy_var_2)
                                                            (True,False) -> myErr ("Participant " ++ happy_var_2 ++ " cannot be the selector")
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  0 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Qes ((checkToken TokenqeS happy_var_1) ++ (checkToken TokenqeS happy_var_3)), S.union (snd happy_var_1) (snd happy_var_3))
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
		 (case ((isPtp happy_var_2), (S.member happy_var_2 (snd happy_var_4))) of
                                       (True, True)  -> (Per happy_var_2 (fst happy_var_4)  happy_var_6, (snd happy_var_4))
                                       (False, _)    -> myErr ("Bad name " ++ happy_var_2)
                                       (True, False) -> myErr ("Participant " ++ happy_var_2 ++ " is not in the loop")
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  0 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (( happy_var_2 )
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  1 happyReduction_8
happyReduction_8 _
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  1 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn5
		 (happy_var_1 ++ " " ++ happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  2 happyReduction_10
happyReduction_10 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn6
		 (if (isPtp happy_var_1) then [happy_var_1] else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  2 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn6
		 (if (isPtp happy_var_1)
                                  then (case happy_var_3 of
                                        [] ->  [happy_var_1]
                                        (s:l) -> (happy_var_1:s:l))
                                  else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 18 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenStr happy_dollar_dollar -> cont 1;
	TokenGrd -> cont 2;
	TokenArr -> cont 3;
	TokenMAr -> cont 4;
	TokenraP -> cont 5;
	TokenBra -> cont 6;
	TokenqeS -> cont 7;
	TokenSec -> cont 8;
	TokenBro -> cont 9;
	TokenBrc -> cont 10;
	TokenCom -> cont 11;
	TokenCurlyo -> cont 12;
	TokenCurlyc -> cont 13;
	TokenarB -> cont 14;
	TokenPer -> cont 15;
	TokenEnd -> cont 16;
	TokenUnl -> cont 17;
	_ -> happyError' (tk:tks)
	}

happyError_ 18 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

rgggrammar tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
    '[':r                     -> lexer $ tail (L.dropWhile (\c->c/=']') r)   -- multi-line comment
    '.':'.':r                 -> lexer $ tail (L.dropWhile (\c->c/='\n') r)  -- single-line comment
    ' ':r                     -> lexer r
    '\n':r                    -> lexer r
    '\t':r                    -> lexer r
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
-- checkToken 'flattens', parallel and sequential composition
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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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
                                     happyFail i tk st
                (-1)          -> {- nothing -}
                                     happyAccept i tk st
                n | (n < ((0) :: Int)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((negate ((n + ((1) :: Int)))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n - ((1) :: Int))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off + i)
         check  = if (off_i >= ((0) :: Int))
                  then (indexShortOffAddr happyCheck off_i ==  i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off








-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

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
     = happyFail (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off + nt)
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
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off + nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (0) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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
happyFail  i tk (action) sts stk =
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
