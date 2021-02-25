{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module SystemParser where
import CFSM
import Misc
import Data.List as L
import Data.Set as S
import Data.Map.Strict as M
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,73) ([0,512,0,64,32,0,0,0,16384,4096,0,4096,0,32768,0,8,0,1,0,0,4096,0,256,0,0,0,10498,16384,0,0,0,0,0,2048,0,0,0,4224,0,3,512,40,8256,5,8,0,0,0,256,0,1,128,0,16,0,10242,16386,0,2048,160,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_sysgrammar","Sys","Mdec","D","M","cho","B","pre","ptps","str","'='","\"||\"","':'","'!'","'?'","'+'","'*'","';'","','","'('","')'","\"tau\"","\"do\"","\"system\"","\"of\"","\"end\"","%eof"]
        bit_start = st * 29
        bit_end = (st + 1) * 29
        read_bit = readArrayBit happyExpList
        bits = L.map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..28]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,40) ([-4,-4,15,20,23,39,37,32,42,43,0,44,46,0,2,45,0,0,38,0,-8,-1,6,2,48,0,40,47,49,50,1,52,6,0,0,0,0,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,40) ([55,0,0,0,0,51,0,0,53,7,0,0,0,0,17,33,0,0,0,0,0,0,24,21,0,0,0,0,0,0,27,0,31,0,0,0,0,0,0,0,0
	])

happyAdjustOffset :: Int -> Int
happyAdjustOffset = id

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,40) ([0,0,0,0,0,0,0,-20,0,0,-2,-3,0,-21,0,0,-4,-5,-6,-8,-12,0,0,0,-19,-18,0,-7,0,0,-13,0,0,-9,-14,-10,-11,-17,-16,-15
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,73) ([-1,9,1,1,5,6,14,1,1,2,8,15,11,11,13,13,1,11,17,13,3,4,5,6,3,4,5,6,4,5,6,4,5,6,1,2,5,6,18,16,1,4,10,1,1,7,1,3,2,1,1,1,12,1,7,0,-1,-1,7,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,73) ([0,31,22,22,29,30,32,22,10,11,23,3,24,24,25,25,5,24,37,25,17,18,19,20,26,18,19,20,27,19,20,35,19,20,16,11,33,20,-1,6,8,10,9,8,13,33,13,16,15,26,39,38,40,35,33,3,0,0,6,0,13,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 20) [
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
	(20 , happyReduce_20)
	]

happy_n_terms = 19 :: Int
happy_n_nonterms = 8 :: Int

happyReduce_1 = happyReduce 6 0 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (if isPtp happy_var_2
                                        then let (sys, ptps) = systemFrom happy_var_4 happy_var_6 in (L.map (simpleStates 0 "") sys, ptps)
                                        else myErr "Name of system invalid: it must start with a letter"
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  1 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  1 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ((happy_var_3 ++ [happy_var_1])
                                        -- if (fst happy_var_1) € (L.map fst happy_var_3)
                                        -- then myErr "Two equations for " ++ (fst happy_var_1)
                                        -- else (happy_var_3 ++ [happy_var_1])
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  2 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn6
		 (if isPtp happy_var_1
--                                  	then checkSelf(happy_var_1, simpleStates 0 happy_var_3)
                                  	then
                                          let m = simpleStates 0 happy_var_1 (renamePtp self happy_var_1 happy_var_3)
                                          in checkSelf(happy_var_1, replaceState (initialOf m) (statePref:happy_var_1) m)
                                  	else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  3 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (choice happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  3 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (cfsmLoop $ choice happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (cfsmPref happy_var_1 (choice happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 _
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (cfsmPref happy_var_1 (emptyMachine 0)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (cfsmPref happy_var_1 (emptyMachine 0)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (cfsmPref happy_var_1 (emptyMachine 0)
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyTerminal (TokenStr happy_var_3))
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (cfsmJump happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyTerminal (TokenStr happy_var_3))
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn10
		 (((self,happy_var_1),Send,happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyTerminal (TokenStr happy_var_3))
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn10
		 (((happy_var_1,self),Receive,happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  6 happyReduction_17
happyReduction_17 (HappyTerminal (TokenStr happy_var_2))
	_
	 =  HappyAbsSyn10
		 (((self,self),Tau,happy_var_2)
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn10
		 (((self,self),Tau,"")
	)

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn11
		 (if (isPtp happy_var_1) then [happy_var_1] else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  7 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn11
		 (if (isPtp happy_var_1) then (happy_var_1: happy_var_3) else myErr ("Bad name " ++ happy_var_1)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 18 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenStr happy_dollar_dollar -> cont 1;
	TokenEqu -> cont 2;
	TokenDec -> cont 3;
	TokenBod -> cont 4;
	TokenSnd -> cont 5;
	TokenRcv -> cont 6;
	TokenBra -> cont 7;
	TokenSta -> cont 8;
	TokenPre -> cont 9;
	TokenCom -> cont 10;
	TokenOrb -> cont 11;
	TokenCrb -> cont 12;
	TokenTau -> cont 13;
	TokenJmp -> cont 14;
	TokenSys -> cont 15;
	TokenDef -> cont 16;
	TokenStp -> cont 17;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 18 tk tks = happyError' (tks, explist)
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
sysgrammar tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
  | TokenBra
  | TokenSta
  | TokenPre
  | TokenCom
  | TokenOrb
  | TokenCrb
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
                   ((s,r),Send,_)    -> (ptp == r)
                   ((s,r),Receive,_) -> (ptp == s)
                   (_,Tau,_)         -> False
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

parseSystem :: String -> String -> System
parseSystem ext txt =
  -- if 'ext' is a valid extension (.fsa, .sys, .cms), returns the parsing of txt as a system of CFSMs 
  case ext of
    ".fsa" -> parseFSA (Prelude.map (\x -> words x) (lines txt))
    ".sys" -> (sysgrammar . lexer) txt
    ".cms" -> (sysgrammar . lexer) txt
    ""     -> parseFSA (Prelude.map (\x -> words x) (lines txt))
    _      -> error ("unknown extension " ++ ext)

myErr :: String -> a
myErr err = error ("sysparser: ERROR - " ++ err)
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
