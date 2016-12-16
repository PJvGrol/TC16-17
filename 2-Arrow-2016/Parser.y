{
module Parser (parse) where
import Scanner
}



%name parse
%tokentype { Token }
%error { parseError }

%token
        "->"        {TNext}
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

Rule  : 

Ident : Id           { Ident $1 }

Cmds  : Cmd          { [$1] }
      | Cmds ',' Cmd { $3 : $1 }

Cmd   : go       { Go }
      | take     { Take }
      | mark     { Mark }
      | nothing  { Nothing2 }
      | turn Dir { Turn $2 }
      | case Dir of Alts end { Case $2 $4 }
      | Id Ident { Ident $2 }
      
Dir   : left     { Left }
      | right    { Right }
      | front    { Front }

Alts  : Alt          { [$1] }
      | Alts ';' Alt { $3 : $1 }
      
Alt   : Pat "->" Cmds { Alt $1 $3 }

Pat   : Empty         { PEmpty }
      | Lambda        { PLambda }
      | Debris        { PDebris }
      | Asteroid      { PAsteroid }
      | Boundary      { PBoundary }
      | '_'           { PDash }
      
{
{-
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
    Semicolon   |
    Empty       |
    Lambda      |
    Debris      |
    Asteroid    |
    Boundary    |
    LDash       |
    Id String
    deriving (Show)-}

data Ident = String

data Program = Program [Rule]
data Rule = Rule Ident Cmds
type Cmds = [Cmd]
data Cmd = Go | Take | Mark | Nothing2 | Turn Dir | Case Dir Alts | Id Ident
data Dir = Left | Right | Front
type Alts = [Alt]
data Alt = Alt Pat Cmds
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PDash

}
