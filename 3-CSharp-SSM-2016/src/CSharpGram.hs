module CSharpGram where

import Prelude hiding ((<$), (<*), (*>))
import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<*),(*>),(<$))


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat 
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatFor    Expr Expr Expr Stat -- for ( expr ; expr ; expr ) body;
          | StatReturn Expr
          | StatPrint  Expr                -- print ( expr );
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprMeth   Token [Expr]
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised (pExpr 0)
           <|> pMethCall
           <|> (\x _ -> ExprOper (Operator "+=") x (ExprConst (ConstInt 1))) <$> (ExprVar <$> sLowerId) <*> (symbol (Operator "++")) -- Transform ++ to +=1
           <|> (\x _ -> ExprOper (Operator "-=") x (ExprConst (ConstInt 1))) <$> (ExprVar <$> sLowerId) <*> (symbol (Operator "--")) -- Transfrom -- to -=1
           <|> ExprConst <$> sBool
           <|> ExprConst <$> sChar
           
pExpr :: Int -> Parser Token Expr
pExpr n | n <= 7 = chainl (pExpr (n+1)) (ExprOper <$> (pOperator n))
        | otherwise = pExprSimple

pOperator :: Int -> Parser Token Token
pOperator n = satisfy f
            where f (Operator x) = expPrior2 x >= n
                  f x = False

-- The priorities of the various operators
expPrior2 :: String -> Int
expPrior2  op | elem op["=", "+=", "-=", "*=", "/="] = 0
              | op == "||" = 1
              | op == "&&" = 2
              | op == "^" = 3
              | elem op ["==","!="] = 4
              | elem op ["<=","<",">=",">"] = 5
              | elem op ["+","-"] = 6
              | elem op ["*","/","%"] = 7
              | elem op ["++", "--"] = 8

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

-- StatFor can't be parsed with parenthesised, so we check for ( ; ; ) seperately
pStat :: Parser Token Stat
pStat =  StatExpr <$> (pExpr 0) <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised (pExpr 0) <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised (pExpr 0) <*> pStat
     <|> StatFor    <$ symbol KeyFor    <* (symbol POpen) <*> (pExpr 0) <* sSemi <*> (pExpr 0) <* sSemi <*> (pExpr 0) <* (symbol PClose) <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> (pExpr 0)               <*  sSemi
     <|> StatPrint  <$ symbol KeyPrint  <*> parenthesised (pExpr 0) <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pMethCall :: Parser Token Expr
pMethCall = ExprMeth <$> sLowerId <*> parenthesised pArguments
          where pArguments = listOf (pExpr 1) (symbol Comma)

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)