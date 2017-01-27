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
           | KeyFor   | KeyPrint    -- Keyword "for" and "print"
           | KeyWhile | KeyReturn 
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyTrue  | KeyFalse    -- Keywords "true", "false"
           | SComment | MComment    -- Singleline Comment or Multipleline Comment
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | UpperCh   Char         -- uppercase character
           | LowerCh   Char         -- lowercase character
           | ConstInt  Int
           deriving (Eq, Show, Ord)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty

-- Added "print", "for", "true", "false" to terminals
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
    , ( KeyFor    , "for"    )
    , ( KeyReturn , "return" )
    , ( KeyPrint  , "print"  )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    , ( KeyTrue   , "true"   )
    , ( KeyFalse  , "false"  )
    ]

-- Since we throw away CSharp comment, we consider it whitespace
lexWhiteSpace :: Parser Char ()
lexWhiteSpace = () <$ greedy ((() <$ satisfy isSpace) <|> lexSingleComment <|> lexMultipleComment)

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

-- Lex lowercase character, also parsing the ' and throwing those away
lexLowerCh :: Parser Char Token
lexLowerCh = LowerCh <$ token "\'" <*> satisfy isLower <* token "\'"

-- Lex uppercase character, also parsing the ' and throwing those away
lexUpperCh :: Parser Char Token
lexUpperCh = UpperCh <$ token "\'" <*> satisfy isUpper <* token "\'"

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> greedyChoice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]

-- Check for the // that signifies singleline comment, then parse to end of line
lexSingleComment :: Parser Char ()
lexSingleComment = () <$ token "//" <* greedy(satisfy (/= '\n'))

-- Check for the /* that signifies multiline comment, then parse to end of multiline comment
lexMultipleComment :: Parser Char ()
lexMultipleComment = () <$ token "/*" <* lexToEndMC

-- Check whether the token is */, if not parse a symbol and try again
lexToEndMC :: Parser Char String
lexToEndMC = token "*/" <<|> (:) <$> anySymbol <*> lexToEndMC

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

-- Added ++, --, +=, -=, /=, *= to list of operators. They have to be in front to have ++ picked over +
operators :: [String]
operators = ["++", "--", "+=", "-=", "*=", "/=", "+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

-- Added lexLowerCh and lexUpperCh
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

-- Check whether it is a bool
sBool :: Parser Token Token
sBool = satisfy isBool
      where isBool (KeyFalse) = True
            isBool (KeyTrue)  = True
            isBool _          = False

-- Check whether it is a char    
sChar :: Parser Token Token
sChar = satisfy isChar
      where isChar (LowerCh _) = True
            isChar (UpperCh _) = True
            isChar _           = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon