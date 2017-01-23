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

type Env = Map Token (Loc, Int)--[(Token,(Loc,Int))]
data Loc = Mem | Lcl 
    
codeAlgebra :: CSharpAlgebra Code Code (Env -> (Env, Code)) (Env -> ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fPrint, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fMethCall)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

numberOfDecls :: Member -> Int
numberOfDecls = undefined --(MemberM )

fMembDecl :: Decl -> Code
fMembDecl d = [TRAP 0]

fMembMeth :: Type -> Token -> [Decl] -> (Env -> (Env, Code)) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x, LINK 0] ++ snd(s env) ++ [UNLINK, RET] 
                             where f ps = Prelude.foldr op [] ps
                                   op (Decl tp tk) xs = fExprCon (ConstInt 3) env Value ++ xs
                                   env = Prelude.foldr op2 empty ps
                                   op2 (Decl tp tk) mp = M.insert tk (Lcl,(-1)*(size mp)-2) mp
-- TODO: something with ps
 -- Ga variabelen opslaan, houd bij waar ze opgeslagen staan
{-=======
fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [STS (-n)] ++ [AJS (-(n-1))] ++ [RET] --TODO: Finish it
                             where
                             n = length ps
                             envmap = undefined --fromList (zip (Prelude.map declToToken ps) (zip (repeat Lcl) [n..])) -- Lcl [(Token, (Loc, Int))]

declToToken :: Decl -> Token
declToToken (Decl tp tk) = tk-}

-- Environment is mapping van String naar Int (int relatieve locatie aan mp) laatste mp staat op -2
-- TODO: something with ps
-- Decl Type Token
-- Ga variabelen opslaan, houd bij waar ze opgeslagen staan -> Zet param onder markpointer mbv ldc
-- Link om MP te plaatsen
-- Environment om bij te houden waar de param opgeslagen staan
-- LDL: Waarde tov MP


 
 
fStatDecl :: Decl -> Env -> (Env,Code)
fStatDecl (Decl t tk) env = let (vars, code) = (M.insert tk (Lcl, length env + 1) env, [LABEL (show tk), AJS 1])--((tk,(Lcl,length env + 1)):env)
                            in (vars, code)

f :: Env -> Code
f = undefined                            
                            
fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> (Env,Code)
fStatExpr exp env = (env, exp env Value) {-(f env, e Value ++ [pop])
                where f = case lookup env -}

fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env,Code)) -> (Env -> (Env,Code)) -> Env -> (Env,Code)
fStatIf e s1 s2 env = (env, c ++ [BRF (n1 + 2)] ++ d ++ [BRA n2] ++ d2)
    where
        c        = e env Value
        (n1, n2) = (codeSize d, codeSize d2)
        d        = snd(s1 env)
        d2       = snd(s2 env)

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env,Code)) -> Env -> (Env,Code)
fStatWhile e s1 env = (env, [BRA n] ++ d ++ c ++ [BRT (-(n + k + 2))])
    where
        c = e env Value
        (n, k) = (codeSize d, codeSize c)
        d = snd(s1 env)

fStatReturn :: (Env -> ValueOrAddress -> Code) -> Env -> (Env,Code)
fStatReturn e env = (env, e env Value ++ [STR R4])

fPrint :: (Env -> ValueOrAddress -> Code) -> Env -> (Env,Code)
fPrint e env = (env, e env Value ++ [TRAP 0])

fStatBlock :: [Env -> (Env,Code)] -> Env -> (Env,Code)
fStatBlock xs env = (env, concat(Prelude.map snd (temp xs env)))

temp :: [Env -> (Env,Code)] -> Env -> [(Env,Code)]
temp xs env = Prelude.foldr op [] xs
            where op a b = a env : b

fExprCon :: Token -> Env -> ValueOrAddress -> Code
fExprCon (ConstInt n) env va = [LDC n]
fExprCon (KeyTrue) env va = [LDC 1]
fExprCon (KeyFalse) env va = [LDC 0]
fExprCon (UpperCh c) env va = [LDC (ord c)]
fExprCon (LowerCh c) env va = [LDC (ord c)]


fExprVar :: Token -> Env -> ValueOrAddress -> Code
fExprVar t env va = case va of
                        Value -> [LDL (snd(env ! t))]
                        Address -> [LABEL (show t), LABEL ((show.size) env)]--[STL (snd(env ! t))]
                                    

{-let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]-}
{-fExprVar t@(LowerId x) env va = undefined let loc = fromJust (lookup t env) in case va of
                                                           Value    ->  (,[LDL  loc])
                                                           Address  ->  (,[LDLA loc]) -}

fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 env va = e2 env Value ++ e1 env Address
fExprOp (Operator op)  e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes ! op]

fMethCall :: Token -> [Env -> ValueOrAddress -> Code] -> Env -> ValueOrAddress -> Code
fMethCall (LowerId x) xs env va = Prelude.foldr op [Bsr x, AJS (-(length xs)), LDR R4] xs --[LDL 10, Bsr x]
                                where op f c = f env Value ++ c

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]
{-=======
fExprOp :: Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0] -- x = 3 LDS = LoadStack: laad value op SP STA: 
-- LDC 3 LDC1 ADD LDS 0 LDLA 37 STA 0 
{-fExprOp (Operator "+=") e1 e2 va = -- Laadt waarde van e1 ++ e2 Value ++ [ADD, LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator "-=") e1 e2 va = -- Laadt waarde van e1 ++ e2 Value ++ [SUB, LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator "*=") e1 e2 va = -- Laadt waarde van e1 ++ e2 Value ++ [MUL, LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator "/=") e1 e2 va = -- Laadt waarde van e1 ++ e2 Value ++ [DIV, LDS 0] ++ e1 Address ++ [STA 0]-}
fExprOp (Operator op)  e1 e2 va = e1 Value ++ e2 Value ++ (opCodes ! op)


opCodes :: Map String [Instr]
opCodes = fromList [ ("+", [ADD]), ("-", [SUB]),  ("*", [MUL]), ("/", [DIV]), ("%", [MOD])
                   , ("<=", [LE]), (">=", [GE]),  ("<", [LT]),  (">", [GT]),  ("==", [EQ])
                   , ("!=", [NE]), ("&&", [AND]), ("||", [OR]), ("^", [XOR]), ("++", [LDC 1, ADD])
                   , ("--", [LDC 1, SUB])
                   ] -- ++ += *= -- -= -- Aparte fExprOp voor alle operaties??
>>>>>>> origin/master-}

