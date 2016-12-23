module Main where

import Prelude hiding ((<*), (<$),Left,Right)
import ParseLib.Abstract hiding (parse)
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List
import Parser
import Scanner
import Data.Maybe
import Debug.Trace
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
type Heading = (Int, Int)

type Environment = Map (Parser.Ident) Cmds

type Stack       =  Cmds
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

{-
data Ident = Ident String deriving (Show)
[Rule "asd" [], Rule "start" [], Rule "asdf" [Case Left [Alt PDash []],Go]]
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
    s <- readFile "Add.Arrow"
    print $ check $ parsehap $ alexScanTokens s

    
scan :: IO()    
scan = do
    s <- readFile "Add.Arrow"
    putStrLn $ show $ alexScanTokens s
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
check prog = elem "start" (fst3 tuple) && testdup (fst3 tuple) && checkrules (fst3 tuple) (snd3 tuple) && thd3 tuple
      where tuple = foldProgram f prog
            testdup [x] = True
            testdup (x:xs) = notElem x xs && testdup xs
{-check = foldProgram (prog,rule,go,take,mark,not,turn,cas,id,alt)
            where
                --prog xs = concat(map snd xs)
                prog xs = elem "start" (rules xs) && removedup (rules xs) && (checkrules ("":(map fst xs)) (concat(map fst (map snd xs))) || map fst (map snd xs) == [])
                --rule (Ident name) ys = (name, concat ys)
                go = ([],True)
                take = ([],True)
                mark = ([],True)
                not = ([],True)
                turn _ = ([],True)
                cas _ xs = ([],pats xs)
                id s = (s,True)
                alt x _ = x

                rules = map fst
                calls = map snd
                pats xs = elem PDash xs
                pat (Alt x _) = x-}
                

f:: AlgebraProgram ([Ident],[Ident],Bool) (Ident,[Ident],Bool) ([Ident],Bool) (Pat, [Ident], Bool) 
f = (\xs -> (rules xs,(removeduplicates.concat) (calls xs),and (map thd3 xs)),
     \s ys -> (s, (removeduplicates.concat) (map fst ys), and (map snd ys)),
     ([],True),([],True),([],True),([],True),
     \s -> ([],True),
     \dir alts -> ((removeduplicates.concat) (map snd3 alts),(elem PDash (map fst3 alts) || containsall (map fst3 alts)) && foldr (&&) True (map thd3 alts)),
     \s ->([s],True),
     \pat cmds -> (pat,(removeduplicates.concat) (map fst cmds), foldr (&&) True (map snd cmds))
    )
    where rules = map fst3
          calls = map snd3
          calls2 = undefined
          
containsall alts = elem PEmpty alts &&
                              elem PLambda alts &&
                              elem PDebris alts &&
                              elem PBoundary alts &&
                              elem PAsteroid alts


                              
fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a
    
removeduplicates = union []
    
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
toEnvironment :: String -> Environment
toEnvironment s = f
                where
                rs = (parsehap . alexScanTokens) s
                c = check rs
                f | c = foldr (\(Rule i c) -> L.insert i c) L.empty rs
                  | otherwise = L.empty

-- Exercise 9
step :: Environment -> ArrowState -> Step
step env (ArrowState sp pos hd st) | stackIsEmpty st = Done sp pos hd
                                   | otherwise = undefined

newPos :: Pos -> Pos -> Pos
newPos (a, b) (c, d) = (a + c, b + d)

validGo :: Space ->  Pos -> Heading -> Bool
validGo sp po he = f k
                   where
                   k = L.lookup (newPos po he) sp
                   l = printContent (fromJust k)
                   f x | isNothing x = False
                       | l == '.' || l == '\\' || l == '%' = True
                       | otherwise = False

stackIsEmpty :: Stack -> Bool
stackIsEmpty [] = False
stackIsEmpty (x:_) = True

updateSpace :: Space -> Pos -> Contents -> Space
updateSpace sp pos cnt = L.update f pos sp
                    where
                    f x = Just cnt
                    
-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env ast = f (step env ast)
                    where
                    f (Done sp _ _)                  = do putStrLn (printSpace sp)
                                                          putStrLn "Finished running program."
                    f (Ok ast@(ArrowState sp _ _ _)) = do putStrLn (printSpace sp)
                                                          putStrLn "Type 'Y' and hit Enter to continue."
                                                          g env ast
                    f (Fail str)                     = do putStrLn (str)
                                                          putStrLn "Running program Failed"
                    g env ast = do c <- getChar 
                                   if c == 'Y' then interactive env ast else g env ast

addStart :: Environment -> Space -> Pos -> Heading -> ArrowState
addStart env sp pos hd = case x of
                         Nothing -> error "No start commands."
                         Just y -> ArrowState sp pos hd y
                        where
                        x = L.lookup("start") env