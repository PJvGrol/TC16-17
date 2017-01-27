module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Data.Char


data ValueOrAddress = Value | Address
    deriving Show

type Env = M.Map Token (Loc, Int)--[(Token,(Loc,Int))]

data Loc = Mem | Lcl | Param
        deriving Eq
    
codeAlgebra :: CSharpAlgebra Code Code (Env -> Env -> (Env, Code)) (Env -> ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fPrint, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fMethCall)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> (Env -> Env -> (Env, Code)) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x, LINK (length env2 - length ps)] ++ snd(s env env2) ++ [UNLINK, RET] 
                             where {-f ps = Prelude.foldr op [] ps
                                   op (Decl tp tk) xs = fExprCon (ConstInt 3) env Value ++ xs-}
                                   env = foldr op2 M.empty ps
                                   op2 (Decl tp tk) mp = M.insert tk (Param,(-1)*(M.size mp)-2) mp
                                   (env2,code) = s env env2

fStatDecl :: Decl -> Env -> Env -> (Env,Code)
fStatDecl (Decl t tk) env env2 = (M.insert tk (Lcl, nrOfLoc Lcl env + 1) env, [])--((tk,(Lcl,length env + 1)):env)
                                 
-- length env + 1 moet anders -> param op -2, -3 etc, locals op 1, 2, 3 etc.
                                 
                                 
f :: Env -> Code
f = undefined


fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fStatExpr exp env env2 = (env, exp env2 Value)
                   
nrOfLoc :: Loc -> Env -> Int
nrOfLoc loc env = M.size (fst (M.partition (\(x,y)-> x == loc) env))
                            
fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> Env -> (Env,Code)) -> (Env -> Env -> (Env,Code)) -> Env -> Env -> (Env,Code)
fStatIf e s1 s2 env env2 = (combine [env,env3,env4], c ++ [BRF (n1 + 2)] ++ d ++ [BRA n2] ++ d2)
    where
        c         = e env2 Value
        (n1, n2)  = (codeSize d, codeSize d2)
        (env3,d)  = s1 env env2
        (env4,d2) = s2 env env2

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> Env -> (Env,Code)) -> Env -> Env -> (Env,Code)
fStatWhile e s1 env env2 = (combine [env,env3], [BRA n] ++ d ++ c ++ [BRT (-(n + k + 2))])
    where
        c        = e env2 Value
        (n, k)   = (codeSize d, codeSize c)
        (env3,d) = s1 env env2

fStatReturn :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fStatReturn e env env2 = (env, e env2 Value ++ [STR R4])

fPrint :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fPrint e env env2 = (env, e env2 Value ++ [TRAP 0])

fStatBlock :: [Env -> Env -> (Env,Code)] -> Env -> Env -> (Env,Code)
fStatBlock xs env env2 = (combine(map fst ys), concat(map snd ys))
                       where ys = temp xs env env2

combine :: [Env] -> Env
combine [env] = env
combine (env:env2:xs) = combine ((M.union env env3): xs)
                    where env3 = snd (M.mapAccum accum add env2)
                          add = nrOfLoc Lcl env
                          accum :: Int -> (Loc, Int) -> (Int, (Loc,Int))
                          accum a (loc,b) | loc == Lcl = (a,(loc,b+a))
                                          | loc == Param = (a-1,(loc,b))

                  --combine xs = snd ( M.mapAccum accum 0 (M.unions xs))                  
                  {-

foldl op M.empty xs
           where op :: Env -> Env -> Env
                 op = -}
                       
temp :: [Env -> Env -> (Env,Code)] -> Env -> Env -> [(Env,Code)]
temp xs env env2 = Prelude.foldr op [] xs
            where op a b = a env env2: b

fExprCon :: Token -> Env -> ValueOrAddress -> Code
fExprCon (ConstInt n) env va = [LDC n]
fExprCon (KeyTrue) env va = [LDC 1]
fExprCon (KeyFalse) env va = [LDC 0]
fExprCon (UpperId c) env va = [LDC (ord (head c))]
fExprCon (LowerId c) env va = [LDC (ord (head c))]

fExprVar :: Token -> Env -> ValueOrAddress -> Code
fExprVar t env va = case va of
                        Value ->   case y of
                                   Mem -> undefined
                                   otherwise -> [LDL z]
                        Address -> case y of
                                   Mem -> undefined
                                   otherwise -> [STL z]
                    where
                    x = env M.! t
                    y = fst x
                    z = snd x                    -- env M.! t = loc,int                 --[LABEL (show t), LABEL ((show . M.size) env)]--
                                    

{-let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]-}
{-fExprVar t@(LowerId x) env va = undefined let loc = fromJust (lookup t env) in case va of
                                                           Value    ->  (,[LDL  loc])
                                                           Address  ->  (,[LDLA loc]) -}

{-fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 env va = e2 env Value ++ e1 env Address
fExprOp (Operator op)  e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes M.! op]
-}
fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 env va = e2 env Value ++ e1 env Address -- x = 3
fExprOp (Operator "+=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [ADD] ++ e1 env Address
fExprOp (Operator "-=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [SUB] ++ e1 env Address
fExprOp (Operator "*=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [MUL] ++ e1 env Address
fExprOp (Operator "/=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [DIV] ++ e1 env Address
fExprOp (Operator op)  e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes M.! op]

fMethCall :: Token -> [Env -> ValueOrAddress -> Code] -> Env -> ValueOrAddress -> Code
fMethCall (LowerId x) xs env va = Prelude.foldr op [Bsr x, AJS (-(length xs)), LDR R4] xs --[LDL 10, Bsr x]
                                where op f c = f env Value ++ c

                                
opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]