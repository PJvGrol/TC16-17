{
module Parser (parsehap, Program, Rule(..), Cmds, Cmd(..),Dir(..), Alts, Alt(..), Pat(..)) where
import Scanner
import Prelude hiding (Left, Right)
}



%name parsehap
%tokentype { TToken }
%error { parseError }

%token
        Next        {TNext}
        '.'         {TDot}
        ','         {TComma}
        go          {TGo}
        take        {TTake}
        mark        {TMark}
        nothing     {TNothing}
        turn        {TTurn}
        case        {TCase}
        of          {TOf}
        end         {TEnd}
        left        {TLeft}
        right       {TRight}
        front       {TFront}
        ';'         {TSemicolon}
        Empty       {TEmpty}
        Lambda      {TLambda}
        Debris      {TDebris}
        Asteroid    {TAsteroid}
        Boundary    {TBoundary}
        '_'         {TLDash}
        Id          {TIdent $$}
        
%%

Program : {- empty -}       {[]}
        | Program Rule      { $1 ++ [$2]  }

Rule  : Ident Next Cmds '.' {Rule $1 $3}

Ident : Id           { Ident $1 }

Cmds  : {- empty -}         { [] }
      | Cmds ',' Cmd        { $1 ++ [$3] }

Cmd   : go       { Go }
      | take     { Take }
      | mark     { Mark }
      | nothing  { Nothing2 }
      | turn Dir { Turn $2 }
      | case Dir of Alts end { Case $2 $4 }
      | Ident    { Id $1 }
      
Dir   : left     { Left }
      | right    { Right }
      | front    { Front }

Alts  : {- empty -}         { [] }
      | Alts ';' Alt        { $1 ++ [$3] }
      
Alt   : Pat Next Cmds { Alt $1 $3 }

Pat   : Empty         { PEmpty }
      | Lambda        { PLambda }
      | Debris        { PDebris }
      | Asteroid      { PAsteroid }
      | Boundary      { PBoundary }
      | '_'           { PDash }
      
{

parseError :: [TToken] -> a
parseError _ = error "Parse error"


data Ident = Ident String deriving (Show)

type Program = [Rule]
data Rule = Rule Ident Cmds deriving (Show)
type Cmds = [Cmd]
data Cmd = Go | Take | Mark | Nothing2 | Turn Dir | Case Dir Alts | Id Ident deriving (Show)
data Dir = Left | Right | Front deriving (Show)
type Alts = [Alt]
data Alt = Alt Pat Cmds deriving (Show)
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PDash deriving (Show)

}
