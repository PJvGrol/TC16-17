module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract hiding (parse)
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Parser
import Scanner
{-data Token =
    Next        |
    Dot         |
    Comma       |
    Go          |
    Take        |
    Mark        |
    Nothing     |
    Turn        |
    Case        |
    Of          |
    End         |
    Left        |
    Right       |
    Front       |
    Semicolon   |
    Empty       |
    Lambda      |
    Debris      |
    Asteroid    |
    Boundary    |
    LDash       |
    Id String
    deriving (Show)-}

type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
--type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map (Parser.Ident) Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

{---data Program = Program [Rule]
--data Rule = Rule Ident Cmds
data Cmds = Epsilon | Cmd [Cmd]
--data Cmd = Go | Take | Mark | Nothing | Turn Dir | Case Dir Alts | Id Ident
data Dir = Left | Right | Front
data Alts = Epsilon2 | Alts Alt [Alt]
--data Alt = Alt Pat Cmds
data Pat = Empty2

data Ident = Ident String deriving (Show)
[Rule (Ident "asd") [], Rule (Ident "start") [], Rule (Ident "asdf") [Id (Ident "asd"),Go]]
type Program = [Rule]
data Rule = Rule Ident Cmds deriving (Show)
type Cmds = [Cmd]
data Cmd = Go | Take | Mark | Nothing2 | Turn Dir | Case Dir Alts | Id Ident deriving (Show)
data Dir = Left | Right | Front deriving (Show)
type Alts = [Alt]
data Alt = Alt Pat Cmds deriving (Show)
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PDash deriving (Show)
-}
main = do
    s <- getContents
    print ((parsehap.scan) s)

-- Exercise 4
{-
    ".. Happy is more efficient at parsing left-recursive rules; they result in a constant stack-space parser, whereas right-recursive rules require stack space
    proportional to the length of the list being parsed. This can be extremely important where long sequences are involved, for instance in automatically
    generated output. For example, the parser in GHC used to use right-recursion to parse lists, and as a result it failed to parse some Happy-generated 
    modules due to running out of space."
    "If a grammar is left-recursive, it has to be transformed into a non left-recursive grammar before we can construct a combinator parser."
    So we can conclude that Happy can easily handle a left-recursive grammar, and might fail in the case of right-recursive grammars. That means that to
    ensure all grammars are handled, any right-recursive grammar has to be transformed into a non right-recursive, or left-recursive grammar. Conversely,
    parser combinators require the transformation of left-recursive grammars into non left-recursive grammars. Therefor we can conclude that parser combinators
    and Happy require the exact opposite recursion.
-}

-- Exercise 5

type AlgebraProgram p r c a = ([r] -> p,
                               Parser.Ident -> [c] -> r,
                               c, c, c, c,
                               Dir -> c,
                               Dir -> [a] -> c,
                               Parser.Ident -> c,
                               Pat -> [c] -> a)
foldProgram :: AlgebraProgram p r c a -> Program -> p
foldProgram (prog,rule,go,take,mark,not,turn,cas,id,alt) = f
            where f xs = prog (map f' xs)
                  f' (Rule ident cmds) = rule ident (map f'' cmds)
                  f'' Go = go
                  f'' Take = take
                  f'' Mark = mark
                  f'' Nothing2 = not
                  f'' (Turn d) = turn d
                  f'' (Case d alts) = cas d (map f''' alts)
                  f'' (Id ident) = id ident
                  f''' (Alt p cmds) = alt p (map f'' cmds)
            
-- Exercise 6
check :: Program -> Bool
check = foldProgram (prog,rule,go,take,mark,not,turn,cas,id,alt)
            where
                --prog xs = concat(map snd xs)
                prog xs = elem "start" (rules xs) && removedup (rules xs) && (checkrules ("":(map fst xs)) (concat(map (fst.snd) xs)) || map (fst.snd) xs == [])
                --rule (Ident name) ys = (name, concat ys)
                go = ([],True)
                take = ([],True)
                mark = ([],True)
                not = ([],True)
                turn _ = ([],True)
                cas _ xs = ([],pats xs)
                id s = (s,True)
                alt x _ = x
                removedup [x] = True
                removedup (x:xs) = notElem x xs && removedup xs
                rules = map fst
                calls = map snd
                pats xs = elem PDash xs
                pat (Alt x _) = x
                

checkrules rules [] = True
checkrules rules [call] = elem call rules
checkrules rules (call:calls) = elem call rules && checkrules rules calls
                
rule name ys = (name, ys)        
-- Exercise 7

printSpace :: Space -> String
printSpace sp = concat (map f ls)
              where
              ls = L.toList sp
              maxx = fst (fst (L.findMax sp))
              maxy = snd (fst (L.findMax sp))
              f x | (fst (fst x) + 1) `mod` (maxx + 1) == 0 = printContent (snd x) : ['\n']
                  | otherwise = [printContent (snd x)]

printContent :: Contents -> Char
printContent Empty = '.'
printContent Lambda = '\\'
printContent Debris = '%'
printContent Asteroid = 'o'
printContent Boundary = '#'

-- Exercise 8

{-toEnvironment :: String -> Environment
toEnvironment s = f
                where
                rs = (parsehap . scan) s
                c = check rs
                f | c = progToEnv rs
                  | otherwise = L.empty
-}                  
progToEnv :: Program -> Environment
progToEnv p = L.empty

-- Exercise 9
step :: Environment -> ArrowState -> Step
step env (ArrowState sp pos hd st) | stackIsEmpty st = Done sp pos hd
                                   | otherwise = undefined

stackIsEmpty :: Stack -> Bool
stackIsEmpty st = True 

