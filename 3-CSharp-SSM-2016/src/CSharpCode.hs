module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Data.Char


data ValueOrAddress = Value | Address
    deriving Show

type Env = Map Token (Loc, Int)

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
fMembDecl d = [TRAP 0]

fMembMeth :: Type -> Token -> [Decl] -> (Env -> Env -> (Env, Code)) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x, LINK (length env2 - length ps)] ++ snd(s env env2) ++ [UNLINK, RET] 
                             where {-f ps = Prelude.foldr op [] ps
                                   op (Decl tp tk) xs = fExprCon (ConstInt 3) env Value ++ xs-}
                                   env = Prelude.foldr op2 empty ps
                                   op2 (Decl tp tk) mp = M.insert tk (Lcl,(-1)*(size mp)-2) mp
                                   (env2,code) = s env env2

fStatDecl :: Decl -> Env -> Env -> (Env,Code)
fStatDecl (Decl t tk) env env2 = let (env, code) = (M.insert tk (Lcl, nrOfLoc Lcl env) env, [LABEL (show tk), AJS 1])
                                 in (env, code)


fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fStatExpr exp env env2 = (env, exp env2 Value)
                   
nrOfLoc :: Loc -> Env -> Int
nrOfLoc loc env = size (fst (M.partition (\(x,y)-> x == loc) env))
                            
fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> Env -> (Env,Code)) -> (Env -> Env -> (Env,Code)) -> Env -> Env -> (Env,Code)
fStatIf e s1 s2 env env2 = (env, c ++ [BRF (n1 + 2)] ++ d ++ [BRA n2] ++ d2)
    where
        c        = e env2 Value
        (n1, n2) = (codeSize d, codeSize d2)
        d        = snd(s1 env env2)
        d2       = snd(s2 env env2)

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> Env -> (Env,Code)) -> Env -> Env -> (Env,Code)
fStatWhile e s1 env env2 = (env, [BRA n] ++ d ++ c ++ [BRT (-(n + k + 2))])
    where
        c = e env2 Value
        (n, k) = (codeSize d, codeSize c)
        d = snd(s1 env env2)

fStatReturn :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fStatReturn e env env2 = (env, e env2 Value ++ [STR R4])

fPrint :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fPrint e env env2 = (env, e env2 Value ++ [TRAP 0])

fStatBlock :: [Env -> Env -> (Env,Code)] -> Env -> Env -> (Env,Code)
fStatBlock xs env env2 = (env, concat(Prelude.map snd (temp xs env env2)))

temp :: [Env -> Env -> (Env,Code)] -> Env -> Env -> [(Env,Code)]
temp xs env env2 = Prelude.foldr op [] xs
            where op a b = a env env2: b

fExprCon :: Token -> Env -> ValueOrAddress -> Code
fExprCon (ConstInt n) env va = [LDC n]
fExprCon (KeyTrue) env va = [LDC 1]
fExprCon (KeyFalse) env va = [LDC 0]
fExprCon (UpperCh c) env va = [LDC (ord c)]
fExprCon (LowerCh c) env va = [LDC (ord c)]

fExprVar :: Token -> Env -> ValueOrAddress -> Code
fExprVar t env va = case va of
                        Value -> [LDL (snd(env ! t))]
                        Address -> [LABEL (show t), LABEL ((show.size) env)]

fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 env va = e2 env Value ++ e1 env Address -- x = 3
-- fExprOp (Operator "+=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [ADD] ++ e1 env Address
-- fExprOp (Operator "-=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [SUB] ++ e1 env Address
-- fExprOp (Operator "*=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [MUL] ++ e1 env Address
-- fExprOp (Operator "/=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [DIV] ++ e1 env Address
fExprOp (Operator op)  e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes ! op]

fMethCall :: Token -> [Env -> ValueOrAddress -> Code] -> Env -> ValueOrAddress -> Code
fMethCall (LowerId x) xs env va = Prelude.foldr op [Bsr x, AJS (-(length xs)), LDR R4] xs --[LDL 10, Bsr x]
                                where op f c = f env Value ++ c

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]