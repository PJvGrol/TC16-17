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


-- Simple testcases for Environment, ArrowState and Space
env = L.singleton "as" [Case Left [Alt PDash []],Go]
ast = ArrowState space (0,0) (0) [Case Left [Alt PDash []],Go, Mark]
space = (L.insert (1,1) Lambda (L.insert (0,1) Boundary ((L.insert (1,0) Debris (L.singleton (0,0) Asteroid)))))

-- 0 = North, 1 = East, 2 = South, 3 = West
type Heading = Int

type Environment = Map (Parser.Ident) Cmds

type Stack       =  Cmds
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

main = do
    s <- readFile "Add.Arrow"
    print $ parsehap $ alexScanTokens s

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
-- Checks whether a given Program is valid.
check :: Program -> Bool
check prog = elem "start" (fst3 tuple) && testdup (fst3 tuple) && checkrules (fst3 tuple) (snd3 tuple) && thd3 tuple
      where tuple = foldProgram f prog
            testdup [x] = True
            testdup (x:xs) = notElem x xs && testdup xs                

f:: AlgebraProgram ([Ident],[Ident],Bool) (Ident,[Ident],Bool) ([Ident],Bool) (Pat, [Ident], Bool) 
f = (\xs -> (mf3 xs,remdupc (ms3 xs),and (mt3 xs)),
     \s ys -> (s, remdupc (map fst ys), and (map snd ys)),
     ([],True),([],True),([],True),([],True),
     \s -> ([],True),
     \dir alts -> (remdupc (ms3 alts),(elem PDash (mf3 alts) || containsall (mf3 alts)) && and (mt3 alts)),
     \s ->([s],True),
     \pat cmds -> (pat,remdupc $ mf cmds, and $ ms cmds)
    )
    where mf3 = map fst3
          ms3 = map snd3
          mt3 = map thd3
          mf = map fst
          ms = map snd
          calls = map snd3
          calls2 = undefined
          remdupc = removeduplicates.concat

-- Checks whether all Alt are contained in given Alts
containsall alts = elem PEmpty alts &&
                    elem PLambda alts && 
                    elem PDebris alts && 
                    elem PBoundary alts && 
                    elem PAsteroid alts

-- Get the first/second/third argument of a 3-tuple                           
fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a
    
removeduplicates = union []
    
checkrules rules [] = True
checkrules rules [call] = elem call rules
checkrules rules (call:calls) = elem call rules && checkrules rules calls
                
rule name ys = (name, ys)
    
-- Exercise 7
-- Print the size of the Space (the maxKey), and then the Space itself via a recursive function call, which prints the Space line by line.
printSpace :: Int -> Space -> String
printSpace i sp = show (fst (L.findMax sp)) ++ "\n" ++ f i
                where
                maxy = snd (fst (L.findMax sp))
                f x | x <= maxy = printLine x sp ++ f (x+1)
                    | otherwise = ""

-- Prints a line of Contents with the same y coordinate
printLine :: Int -> Space -> String
printLine i sp = (map printContent (map snd (filter (checkLine i) (L.toList sp)))) ++ "\n"

-- Used to filter on a certain y coordinate
checkLine :: Int -> ((Int, Int), Contents) -> Bool
checkLine i ((_, x),_) = x == i

-- Show instance for Contents
instance Show Contents where
    show Empty = "."
    show Lambda = "\\"
    show Debris = "%"
    show Asteroid = "o"
    show Boundary = "#"

-- Make a Char of the Show instance
printContent :: Contents -> Char
printContent x = (head.show) x

-- Exercise 8
-- Translate the given string into an environment. We first scan and parse the given string, then we check whether the result is valid.
-- Finally we fold the rules and insert them into Environment (which starts as an empty Map). If the result is not valid, we return an empty Map
toEnvironment :: String -> Environment
toEnvironment s = f
                where
                rs = (parsehap . alexScanTokens) s
                c = check rs
                f | c = foldr (\(Rule i c) -> L.insert i c) L.empty rs
                  | otherwise = L.empty

-- Exercise 9
-- Do a step, given Environment and ArrowState. When the stack is empty, we're Done, otherwise we can analyze the command and act on it.
step :: Environment -> ArrowState -> Step
step env (ArrowState sp pos hd st) | null st = Done sp pos hd
                                   | otherwise = analyze env (head st) (ArrowState sp pos hd (tail st))

-- Returns a new Pos, by taking one step in the direction of the Heading.
newPos :: Pos -> Heading -> Pos
newPos (a,b) 0 = (a,b+1)
newPos (a,b) 1 = (a+1,b)
newPos (a,b) 2 = (a,b-1)
newPos (a,b) 3 = (a-1,b)

-- Checks whether the conditions for a valid Go hold (target field is empty, or contains lambda or debris)
validGo :: Space ->  Pos -> Heading -> Bool
validGo sp po he = f k
                   where
                   k = L.lookup (newPos po he) sp
                   l = printContent (fromJust k)
                   f x | isNothing x = False
                       | l == '.' || l == '\\' || l == '%' = True
                       | otherwise = False

-- Updates a certain Pos in the Space by replacing current Content with given Content.
updateSpace :: Space -> Pos -> Contents -> Space
updateSpace sp pos cnt = L.update f pos sp
                    where
                    f x = Just cnt

-- Pattern match on the various Cmd. Then perform the action described in the Cmd.
-- Since the check on the Stack is performed in step, we have to return either the Ok or Fail ArrowState.
-- The action will Fail when, in the case of a rule call the rule doesn't exist in the environment,
-- or in the case of the Case argument, there is no matching Alt for the Content that is being looked at.
analyze :: Environment -> Cmd -> ArrowState -> Step
analyze _   Go         ast@(ArrowState sp pos hd st) | validGo sp pos hd = Ok (ArrowState sp (newPos pos hd) hd st)
                                                     | otherwise         = Ok ast
analyze _   Take           (ArrowState sp pos hd st)                     = Ok (ArrowState (takePos sp pos) pos hd st)
analyze _   Mark           (ArrowState sp pos hd st)                     = Ok (ArrowState (updateSpace sp pos Lambda) pos hd st)
analyze _   Nothing2   ast@(ArrowState sp pos hd st)                     = Ok ast
analyze _   (Turn dir)     (ArrowState sp pos hd st)                     = Ok (ArrowState sp pos (turn dir hd) st)
analyze env (Id rule)      (ArrowState sp pos hd st)                     = case x of
                                                                           Nothing -> Fail "Rule doesn't exist."
                                                                           Just y -> Ok (ArrowState sp pos hd (y++st))
                                                                         where 
                                                                         x = L.lookup rule env
analyze _   (Case dir alts) (ArrowState sp pos hd st)                    = f xs
                                                                         where
                                                                         xs = filter (checkAlts (printContent (isInMap sp (lookAtPos pos dir hd)))) alts
                                                                         f ys | null ys = Fail "No matching alts."
                                                                              | otherwise = Ok (ArrowState sp pos hd (addCmds (head ys) st))

-- Add the Cmds in the given Alt to the given Stack
addCmds :: Alt -> Stack -> Stack
addCmds (Alt _ cmds) st = cmds ++ st

-- Checks whether the Alt matches a given Content (Content is given as string)                                 
checkAlts :: Char -> Alt -> Bool
checkAlts x (Alt PEmpty cmd) = x == '.'
checkAlts x (Alt PLambda cmd) = x == '\\'
checkAlts x (Alt PDebris cmd) = x == '%'
checkAlts x (Alt PAsteroid cmd) = x== '0'
checkAlts x (Alt PBoundary cmd) = x == '#'
checkAlts x (Alt PDash cmd) = True

-- Find the position that is being looked at, based on current Pos, and given Heading and Dir
lookAtPos :: Pos -> Dir -> Heading -> Pos
lookAtPos pos dir hd = newPos pos (turn dir hd)

-- Check whether a given Pos is contained in the Space, and what the Content is.
-- If not included in the Space, this should imply that the position is a Boundary.
isInMap :: Space -> Pos -> Contents
isInMap sp pos = case x of
               Nothing -> Boundary
               Just y -> y
               where
               x = L.lookup pos sp

-- Perform the take action on given Position; pick up a Lambda or Debris and replace it by Empty. If it's not one of those two, do nothing.
-- If the position is not in the Space, return an error.
takePos :: Space -> Pos -> Space
takePos sp pos = case x of
                 Nothing -> error "Invalid position"
                 Just y -> f y
               where
               x = L.lookup pos sp
               f Lambda = updateSpace sp pos Empty
               f Debris = updateSpace sp pos Empty
               f _ = sp

-- Perform the turn action, using given Dir and Heading.
turn :: Dir -> Heading -> Heading
turn Left x = (x - 1) `mod` 4
turn Right x = (x + 1) `mod` 4
turn Front x = x

-- Exercise 10
{-
    A recursive call in the middle of the command sequence requires more memory space. (???)
-}
                    
-- Exercise 11
-- Run the program interactively, using recursion. When the program is finished we print the Space and show that the program has finished running.
-- Upon a Fail, we print the reason for the Fail and mention that the program has stopped running
-- Upon the Ok ArrowState, we print the current Space, and ask for input to confirm that the program must continue running. The program won't start running
-- again, until and 'Y' has been entered.
interactive :: Environment -> ArrowState -> IO ()
interactive env ast = f (step env ast)
                    where
                    f (Done sp _ _)                  = do putStrLn (printSpace 0 sp)
                                                          putStrLn "Finished running program."
                    f (Ok ast@(ArrowState sp _ _ _)) = do putStrLn (printSpace 0 sp)
                                                          putStrLn "Type 'Y' and hit Enter to continue."
                                                          g env ast
                    f (Fail str)                     = do putStrLn (str)
                                                          putStrLn "Running program Failed"
                    g env ast = do c <- getChar 
                                   if c == 'Y' then interactive env ast else g env ast

-- Bonus 14
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env ast = f (step env ast)
              where
              f (Done sp pos hd) = (sp, pos, hd)
              f (Ok ast) = interactive env ast
              f (Fail str) = error str

-- Allows us to, if needed, create an intial ArrowState which puts on the stack the commands included under "start".
addStart :: Environment -> Space -> Pos -> Heading -> ArrowState
addStart env sp pos hd = case x of
                         Nothing -> error "No start commands."
                         Just y -> ArrowState sp pos hd y
                        where
                        x = L.lookup("start") env