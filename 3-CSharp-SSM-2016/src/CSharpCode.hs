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

codeAlgebra :: CSharpAlgebra Code Code Code (ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fPrint, fStatBlock)
    , (fExprCon, fExprVar, fExprOp)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [STS (-n)] ++ [AJS (-(n-1))] ++ [RET] --TODO: Finish it
                             where
                             n = length ps

                             -- Environment is mapping van String naar Int
-- TODO: something with ps
-- Decl Type Token
 -- Ga variabelen opslaan, houd bij waar ze opgeslagen staan -> Zet param onder markpointer mbv ldc
 -- Link om MP te plaatsen
 -- Environment om bij te houden waar de param opgeslagen staan
 
fStatDecl :: Decl -> Code
fStatDecl d = []

fStatExpr :: (ValueOrAddress -> Code) -> Code
fStatExpr e = e Value ++ [pop]

fStatIf :: (ValueOrAddress -> Code) -> Code -> Code -> Code
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: (ValueOrAddress -> Code) -> Code -> Code
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

fStatReturn :: (ValueOrAddress -> Code) -> Code
fStatReturn e = e Value ++ [pop] ++ [RET]

fPrint :: (ValueOrAddress -> Code) -> Code
fPrint e = e Value ++ [TRAP 0]

fStatBlock :: [Code] -> Code
fStatBlock = concat

fExprCon :: Token -> ValueOrAddress -> Code
fExprCon (ConstInt n) va = [LDC n]
fExprCon (KeyTrue) va = [LDC 1]
fExprCon (KeyFalse) va = [LDC 0]
fExprCon (UpperCh c) va = [LDC (ord c)]
fExprCon (LowerCh c) va = [LDC (ord c)]

fExprVar :: Token -> ValueOrAddress -> Code
fExprVar (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator op)  e1 e2 va = e1 Value ++ e2 Value ++ [opCodes ! op]


opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

