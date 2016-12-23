{-# OPTIONS_GHC -w #-}
module Parser (parsehap, Program, Rule(..), Cmds, Cmd(..),Dir(..), Alts, Alt(..), Pat(..), Ident(..)) where
import Scanner
import Prelude hiding (Left, Right)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (TToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (4) = happyGoto action_2
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (36) = happyShift action_5
action_2 (37) = happyAccept
action_2 (5) = happyGoto action_3
action_2 (6) = happyGoto action_4
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (15) = happyShift action_6
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (18) = happyShift action_10
action_6 (19) = happyShift action_11
action_6 (20) = happyShift action_12
action_6 (21) = happyShift action_13
action_6 (22) = happyShift action_14
action_6 (23) = happyShift action_15
action_6 (36) = happyShift action_5
action_6 (6) = happyGoto action_7
action_6 (7) = happyGoto action_8
action_6 (9) = happyGoto action_9
action_6 _ = happyReduce_5

action_7 _ = happyReduce_15

action_8 (16) = happyShift action_23
action_8 _ = happyFail

action_9 (17) = happyShift action_22
action_9 (8) = happyGoto action_21
action_9 _ = happyReduce_7

action_10 _ = happyReduce_9

action_11 _ = happyReduce_10

action_12 _ = happyReduce_11

action_13 _ = happyReduce_12

action_14 (26) = happyShift action_17
action_14 (27) = happyShift action_18
action_14 (28) = happyShift action_19
action_14 (10) = happyGoto action_20
action_14 _ = happyFail

action_15 (26) = happyShift action_17
action_15 (27) = happyShift action_18
action_15 (28) = happyShift action_19
action_15 (10) = happyGoto action_16
action_15 _ = happyFail

action_16 (24) = happyShift action_25
action_16 _ = happyFail

action_17 _ = happyReduce_16

action_18 _ = happyReduce_17

action_19 _ = happyReduce_18

action_20 _ = happyReduce_13

action_21 _ = happyReduce_6

action_22 (18) = happyShift action_10
action_22 (19) = happyShift action_11
action_22 (20) = happyShift action_12
action_22 (21) = happyShift action_13
action_22 (22) = happyShift action_14
action_22 (23) = happyShift action_15
action_22 (36) = happyShift action_5
action_22 (6) = happyGoto action_7
action_22 (9) = happyGoto action_24
action_22 _ = happyFail

action_23 _ = happyReduce_3

action_24 (17) = happyShift action_22
action_24 (8) = happyGoto action_35
action_24 _ = happyReduce_7

action_25 (30) = happyShift action_29
action_25 (31) = happyShift action_30
action_25 (32) = happyShift action_31
action_25 (33) = happyShift action_32
action_25 (34) = happyShift action_33
action_25 (35) = happyShift action_34
action_25 (11) = happyGoto action_26
action_25 (13) = happyGoto action_27
action_25 (14) = happyGoto action_28
action_25 _ = happyReduce_19

action_26 (25) = happyShift action_39
action_26 _ = happyFail

action_27 (29) = happyShift action_38
action_27 (12) = happyGoto action_37
action_27 _ = happyReduce_21

action_28 (15) = happyShift action_36
action_28 _ = happyFail

action_29 _ = happyReduce_24

action_30 _ = happyReduce_25

action_31 _ = happyReduce_26

action_32 _ = happyReduce_27

action_33 _ = happyReduce_28

action_34 _ = happyReduce_29

action_35 _ = happyReduce_8

action_36 (18) = happyShift action_10
action_36 (19) = happyShift action_11
action_36 (20) = happyShift action_12
action_36 (21) = happyShift action_13
action_36 (22) = happyShift action_14
action_36 (23) = happyShift action_15
action_36 (36) = happyShift action_5
action_36 (6) = happyGoto action_7
action_36 (7) = happyGoto action_41
action_36 (9) = happyGoto action_9
action_36 _ = happyReduce_5

action_37 _ = happyReduce_20

action_38 (30) = happyShift action_29
action_38 (31) = happyShift action_30
action_38 (32) = happyShift action_31
action_38 (33) = happyShift action_32
action_38 (34) = happyShift action_33
action_38 (35) = happyShift action_34
action_38 (13) = happyGoto action_40
action_38 (14) = happyGoto action_28
action_38 _ = happyFail

action_39 _ = happyReduce_14

action_40 (29) = happyShift action_38
action_40 (12) = happyGoto action_42
action_40 _ = happyReduce_21

action_41 _ = happyReduce_23

action_42 _ = happyReduce_22

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Rule happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (Go
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (Take
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (Mark
	)

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn9
		 (Nothing2
	)

happyReduce_13 = happySpecReduce_2  9 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Turn happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Case happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn9
		 (Id happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn10
		 (Left
	)

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn10
		 (Right
	)

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn10
		 (Front
	)

happyReduce_19 = happySpecReduce_0  11 happyReduction_19
happyReduction_19  =  HappyAbsSyn11
		 ([]
	)

happyReduce_20 = happySpecReduce_2  11 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  12 happyReduction_21
happyReduction_21  =  HappyAbsSyn12
		 ([]
	)

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (Alt happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn14
		 (PEmpty
	)

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn14
		 (PLambda
	)

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn14
		 (PDebris
	)

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn14
		 (PAsteroid
	)

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn14
		 (PBoundary
	)

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn14
		 (PDash
	)

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TNext -> cont 15;
	TDot -> cont 16;
	TComma -> cont 17;
	TGo -> cont 18;
	TTake -> cont 19;
	TMark -> cont 20;
	TNothing -> cont 21;
	TTurn -> cont 22;
	TCase -> cont 23;
	TOf -> cont 24;
	TEnd -> cont 25;
	TLeft -> cont 26;
	TRight -> cont 27;
	TFront -> cont 28;
	TSemicolon -> cont 29;
	TEmpty -> cont 30;
	TLambda -> cont 31;
	TDebris -> cont 32;
	TAsteroid -> cont 33;
	TBoundary -> cont 34;
	TLDash -> cont 35;
	TIdent happy_dollar_dollar -> cont 36;
	_ -> happyError' (tk:tks)
	}

happyError_ 37 tk tks = happyError' tks
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
happyError' :: () => [(TToken)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parsehap tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [TToken] -> a
parseError _ = error "Parse error"


type Ident = String

type Program = [Rule]
data Rule = Rule Ident Cmds deriving (Show)
type Cmds = [Cmd]
data Cmd = Go | Take | Mark | Nothing2 | Turn Dir | Case Dir Alts | Id Ident deriving (Show)
data Dir = Left | Right | Front deriving (Show)
type Alts = [Alt]
data Alt = Alt Pat Cmds deriving (Show)
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PDash deriving (Eq,Show)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc14956_0\\ghc_2.h" #-}






















































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
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
