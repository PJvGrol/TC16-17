module CSharpLex where

import Prelude hiding ((<$), (<*), (*>))
import Data.Char
import Control.Monad
import ParseLib.Abstract
import Prelude hiding ((<*),(*>),(<$))


data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn | KeyPrint
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyTrue  | KeyFalse
           | SComment | MComment
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | UpperCh   Char
           | LowerCh   Char
           | ConstInt  Int
           deriving (Eq, Show)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs


greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty


terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyWhile  , "while"  )
    , ( KeyReturn , "return" )
    , ( KeyPrint  , "print"  )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    , ( KeyTrue   , "true"   )
    , ( KeyFalse  , "false"  )
    ]


lexWhiteSpace :: Parser Char ()
lexWhiteSpace = () <$ greedy ((() <$ satisfy isSpace) <|> lexSingleComment <|> lexMultipleComment)

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexLowerCh :: Parser Char Token
lexLowerCh = LowerCh <$> satisfy isLower

lexUpperCh :: Parser Char Token
lexUpperCh = UpperCh <$> satisfy isUpper

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]

lexSingleComment :: Parser Char ()
lexSingleComment = () <$ token "//" <* greedy(satisfy (/= '\n'))

lexMultipleComment :: Parser Char ()
lexMultipleComment = () <$ token "/*" <* greedy(token "*/")


stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "=", "++", "--", "+=", "-=", "*=", "/="]

lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexLowerId
             , lexUpperId
             , lexLowerCh
             , lexUpperCh
             ]

lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace) <* eof


sStdType :: Parser Token Token
sStdType = satisfy isStdType
    where isStdType (StdType _) = True
          isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False
          
sUpperCh :: Parser Token Token
sUpperCh = satisfy isUpperCh
    where isUpperCh (UpperCh _) = True
          isUpperCh _           = False

sLowerCh :: Parser Token Token
sLowerCh = satisfy isLowerCh
    where isLowerCh (LowerCh _) = True
          isLowerCh _           = False

sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst _             = False

sOperator :: Parser Token Token
sOperator = satisfy isOperator
    where isOperator (Operator _) = True
          isOperator _            = False


sSemi :: Parser Token Token
sSemi =  symbol Semicolon