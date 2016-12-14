{
module Main (main) where
}
%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

    $white+                         ;
    "--".*                          ;
    "->"                            { \s -> Next }
    \.                              { \s -> Dot }
    \,                              { \s -> Comma }
    go                              { \s -> Go }
    take                            { \s -> Take }
    mark                            { \s -> Mark }
    nothing                         { \s -> Nothing }
    turn                            { \s -> Turn }
    case                            { \s -> Case }
    of                              { \s -> Of }
    end                             { \s -> End }
    left                            { \s -> Left }
    right                           { \s -> Right }
    front                           { \s -> Front }
    Empty                           { \s -> Empty }
    Lambda                          { \s -> Lambda }
    Debris                          { \s -> Debris }
    Asteroid                        { \s -> Asteroid }
    Boundary                        { \s -> Boundary }
    \_                              { \s -> LDash }
    Ident                           { \s -> Ident }
    
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
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
    Empty       |
    Lambda      |
    Debris      |
    Asteroid    |
    LDash       |
    Ident

data Ident = 
    Letter String  |
    Digit Int      |
    +              |
    -
}