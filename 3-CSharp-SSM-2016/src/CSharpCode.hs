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

type Env = M.Map Token (Loc, Int)

data Loc = Mem | Lcl | Param
        deriving Eq
    
codeAlgebra :: CSharpAlgebra (Env -> Code) (Env -> Env -> (Env,Code)) (Env -> Env -> (Env, Code)) (Env -> ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatFor, fStatReturn, fPrint, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fMethCall)
    )

fClas :: Token -> [Env -> Env -> (Env,Code)] -> Env -> Code
fClas c ms menv = [LDR SP, STR R5, AJS (nrOfLoc Mem menv2), Bsr "main", HALT] ++ (concat.map snd) (f ms menv menv2)
                      where f xs p1 p2 = map (\x -> x p1 p2) xs
                            menv2 = (combineMem.fst) (unzip (f ms menv menv2))

fMembDecl :: Decl -> Env -> Env -> (Env,Code)
fMembDecl (Decl t tk) menv menv2 = (M.insert tk (Mem, M.size menv + 1) menv,[])

fMembMeth :: Type -> Token -> [Decl] -> (Env -> Env -> (Env, Code)) -> Env -> Env -> (Env,Code)
fMembMeth t (LowerId x) ps s menv menv2 = (menv, [LABEL x, LINK (length env2 - length ps - nrOfLoc Mem env2)] ++ snd(s env env2) ++ [UNLINK, RET])
                             where env = foldr op2 menv2 ps
                                   op2 (Decl tp tk) mp = M.insert tk (Param,(-1)*(M.size mp)-2) mp
                                   (env2,code) = s env env2

fStatDecl :: Decl -> Env -> Env -> (Env,Code)
fStatDecl (Decl t tk) env env2 = (M.insert tk (Lcl, nrOfLoc Lcl env + 1) env, [])--((tk,(Lcl,length env + 1)):env)

fStatFor :: (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> Env -> (Env, Code)) -> Env -> Env -> (Env, Code)
fStatFor e1 e2 e3 s1 env1 env2 = (combine [env1, env3], a ++ [BRA (k +m)] ++ d ++ c ++ b ++ [BRT (-(k + l + m + 2))] )
                               where
                               a = e1 env2 Address
                               b = e2 env2 Value
                               c = e3 env2 Address
                               (env3, d) = s1 env1 env2
                               (k, l, m) = (codeSize d, codeSize b, codeSize c)

fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> Env -> (Env,Code)
fStatExpr exp env env2 = (env, exp env2 Value)

-- Checks the amount of variables saved to a certain location (Member variables, parameters or local variables)
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
                                          | otherwise  = (a-1,(loc,b))

combineMem :: [Env] -> Env
combineMem [env] = env
combineMem (env:env2:xs) = combineMem ((M.union env env3): xs)
                      where env3 = snd (M.mapAccum accum add env2)
                            add = M.size env
                            accum :: Int -> (Loc, Int) -> (Int, (Loc,Int))
                            accum a (loc,b) | loc == Mem = (a,(loc,b+a))
                                            | otherwise = (a,(loc,b+10))
                       
temp :: [Env -> Env -> (Env,Code)] -> Env -> Env -> [(Env,Code)]
temp xs env env2 = Prelude.foldr op [] xs
            where op a b = a env env2: b

-- True is saved as 1, False as 0. Char saved as ASCII value, int saved as itself.
fExprCon :: Token -> Env -> ValueOrAddress -> Code
fExprCon (ConstInt n) env va = [LDC n]
fExprCon (KeyTrue) env va = [LDC 1]
fExprCon (KeyFalse) env va = [LDC 0]
fExprCon (UpperCh c) env va = [LDC (ord c)]
fExprCon (LowerCh c) env va = [LDC (ord c)]

-- Since Member variables are loaded from the stack, we have to use separate commands for those
-- Parameters and local variables are loaded based on the MP, so those commands are the same
fExprVar :: Token -> Env -> ValueOrAddress -> Code
fExprVar t env va = case va of
                        Value ->   case y of
                                   Mem -> [LDR R5, LDA z] 
                                   otherwise -> [LDL z]
                        Address -> case y of
                                   Mem -> [LDR R5, STA z]
                                   otherwise -> [STL z]
                    where
                    -- x = the (loc, int) from the gives token (variable)
                    x = env M.! t
                    -- y = loc of variable, z = int of variable (offset to MP/Address)
                    y = fst x
                    z = snd x

-- Value and Address already push/pop what's needed, so no ssm commands have to be added here for that
-- +=,-=,*=,/= simply load the variable, then the constant, do the appropriate operation, and then save the value
fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 env va = e2 env Value ++ e1 env Address
fExprOp (Operator "+=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [ADD] ++ e1 env Address
fExprOp (Operator "-=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [SUB] ++ e1 env Address
fExprOp (Operator "*=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [MUL] ++ e1 env Address
fExprOp (Operator "/=") e1 e2 env va = e1 env Value ++ e2 env Value ++ [DIV] ++ e1 env Address
fExprOp (Operator op)  e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes M.! op]

fMethCall :: Token -> [Env -> ValueOrAddress -> Code] -> Env -> ValueOrAddress -> Code
fMethCall (LowerId x) xs env va = Prelude.foldr op [Bsr x, AJS (-(length xs)), LDR R4] xs
                                where op f c = f env Value ++ c

                                
opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]