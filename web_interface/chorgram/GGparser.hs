{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module GGparser where
import SyntacticGlobalGraphs
import Data.List as L
import Data.Set as S
import Misc
import CFSM
import qualified Data.Array as Happy_Data_Array
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,29) ([4,40,0,-4,23,4,4,2,11,39,38,4,4,4,36,17,16,28,24,22,35,0,0,34,33,32,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,29) ([31,0,0,0,0,30,29,0,0,0,27,18,13,3,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0
	])

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,29) ([0,0,-2,0,0,0,0,0,0,0,0,0,0,0,-7,-6,-5,0,-10,0,0,-9,-8,0,0,0,-4,-11,-3
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,56) ([-1,5,6,0,8,1,2,5,6,1,8,7,16,0,12,11,5,6,0,8,9,5,6,6,8,8,3,4,1,0,0,0,10,1,1,1,1,13,10,1,1,-1,2,-1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,56) ([0,12,13,14,14,5,3,12,13,27,14,6,-1,15,22,7,12,13,16,14,21,12,13,13,14,14,10,11,17,7,8,3,24,27,19,29,23,25,26,19,20,0,3,0,14,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 10) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10)
	]

happy_n_terms = 17 :: Int
happy_n_nonterms = 2 :: Int

happyReduce_1 = happySpecReduce_1  0 happyReduction_1
happyReduction_1 _
	 =  HappyAbsSyn4
		 (myErr "\167 not permitted"
	)

happyReduce_2 = happyReduce 5 0 happyReduction_2
happyReduction_2 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), (isPtp happy_var_3), not(happy_var_1 == happy_var_3)) of
				    (True, True, True)   -> ((Act (happy_var_1 , happy_var_3) happy_var_5), S.fromList [happy_var_1,happy_var_3])
				    (True, False, True)  -> myErr ("Bad name " ++ happy_var_3)
				    (True, _, False)     -> myErr ("A sender " ++ happy_var_3 ++ " cannot be also the receiver")
				    (False, True, True)  -> myErr ("Bad name " ++ happy_var_1)
				    (False, False, True) -> myErr ("Bad names " ++ happy_var_1 ++ " and " ++ happy_var_3)
				    (False, _, False)    -> myErr ("Bad name " ++ happy_var_1 ++ " and sender and receiver must be different")
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 0 happyReduction_3
happyReduction_3 ((HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_1), not(L.elem happy_var_1 happy_var_3)) of
                                  (True, True)   -> case happy_var_3 of
                                                    []   -> myErr (happy_var_1 ++ " cannot be empty") -- (happy_var_1 ++ " => " ++ "[]")
                                                    s:[] -> ((Act (happy_var_1 , s) happy_var_5), S.fromList([happy_var_1,s]))
                                                    _    -> (Par (L.map (\s -> (Act (happy_var_1 , s) happy_var_5)) happy_var_3),S.fromList(happy_var_1:happy_var_3))
                                  (True, False)  -> myErr (happy_var_1 ++ " must be in " ++ (show happy_var_3))
                                  (False, _)     -> myErr ("Bad name " ++ happy_var_1)
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  0 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Par ((checkToken TokenPar happy_var_1) ++ (checkToken TokenPar happy_var_3)), S.union (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  0 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Bra (S.fromList $ (checkToken TokenBra happy_var_1) ++ (checkToken TokenBra happy_var_3)), S.union (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  0 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((Seq ((checkToken TokenSeq happy_var_1) ++ (checkToken TokenSeq happy_var_3)), S.union (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 0 happyReduction_7
happyReduction_7 ((HappyTerminal (TokenStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case ((isPtp happy_var_4), (S.member happy_var_4 (snd happy_var_2))) of
                                    (True, True)  -> (Rep (fst happy_var_2) happy_var_4 , S.union (S.singleton happy_var_4) (snd happy_var_2))
                                    (False, _)    -> myErr ("Bad name " ++ happy_var_4)
                                    (True, False) -> myErr ("Participant " ++ happy_var_4 ++ " is not in the loop")
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  0 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (( happy_var_2 )
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  1 happyReduction_9
happyReduction_9 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn5
		 (if (isPtp happy_var_1) then [happy_var_1] else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  1 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn5
		 (if (isPtp happy_var_1)
                                  then (case happy_var_3 of
                                        [] ->  [happy_var_1]
                                        (s:l) -> (happy_var_1:s:l))
                                  else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 16 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenStr happy_dollar_dollar -> cont 1;
	TokenEmp -> cont 2;
	TokenArr -> cont 3;
	TokenMAr -> cont 4;
	TokenPar -> cont 5;
	TokenBra -> cont 6;
	TokenSta -> cont 7;
	TokenSeq -> cont 8;
	TokenUnt -> cont 9;
	TokenSec -> cont 10;
	TokenBro -> cont 11;
	TokenBrc -> cont 12;
	TokenCom -> cont 13;
	TokenCtb -> cont 14;
	TokenCte -> cont 15;
	_ -> happyError' (tk:tks)
	}

happyError_ 16 tk tks = happyError' tks
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
    ' '  :r   -> lexer r
    '\n' :r   -> lexer r
    '\t' :r   -> lexer r
    '-':'>':r -> TokenArr : (lexer $ tail r)
    '=':'>':r -> TokenMAr : (lexer $ tail r)
    '§':r     -> TokenEmp : lexer r
    '|':r     -> TokenPar : lexer r
    '+':r     -> TokenBra : lexer r
    '*':r     -> TokenSta : lexer r
    '@':r     -> TokenUnt : lexer r
    ':':r     -> TokenSec : lexer r
    ';':r     -> TokenSeq : lexer r
    ',':r     -> TokenCom : lexer r
    '(':r     -> TokenBro : lexer r
    ')':r     -> TokenBrc : lexer r
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
{-# LINE 1 "templates/GenericTemplate.hs" #-}









































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 






data Happy_IntList = HappyCons Int Happy_IntList


















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


indexShortOffAddr arr off = arr Happy_Data_Array.! off








-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



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

