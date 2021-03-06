{
module Parser (parsehap, Program, Rule(..), Cmds, Cmd(..),Dir(..), Alts, Alt(..), Pat(..), Ident(..)) where
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

Rule  : Ident Next Cmds '.' {Rule $1 (reverse $3) }

Ident : Id           { $1 }

Cmds  : {- empty -}         { [] }
      | Cmds2 Cmd      { $2 : $1 }

Cmds2 : {-empty -}    {[]}
      | Cmds2 Cmd ','    { $2 : $1 }

Cmd   : go       { Go }
      | take     { Take }
      | mark     { Mark }
      | nothing  { Nothing2 }
      | turn Dir { Turn $2 }
      | case Dir of Alts end { Case $2 (reverse $4) }
      | Ident    { Id $1 }
      
Dir   : left     { Left }
      | right    { Right }
      | front    { Front }

Alts  : {- empty -}         { [] }
      | Alts2 Alt      { $2 : $1 }

Alts2 : {- empty -}         {[]}
      | Alts2 Alt ';'       {$2 : $1}
      
Alt   : Pat Next Cmds { Alt $1 (reverse $3) }

Pat   : Empty         { PEmpty }
      | Lambda        { PLambda }
      | Debris        { PDebris }
      | Asteroid      { PAsteroid }
      | Boundary      { PBoundary }
      | '_'           { PDash }
      
{

parseError :: [TToken] -> a
parseError _ = error "Parse error"


type Ident = String

type Program = [Rule]
data Rule = Rule Ident Cmds deriving (Show)
type Cmds = [Cmd]
data Cmd = Go | Take | Mark | Nothing2 | Turn Dir | Case Dir Alts | Id Ident deriving (Show)
data Dir = Left | Right | Front deriving (Show)
type Alts = [Alt]
data Alt = Alt Pat Cmds deriving (Show)
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PDash deriving (Eq,Show)

}
