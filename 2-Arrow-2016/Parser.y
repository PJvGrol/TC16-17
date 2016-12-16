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
        Ident       {TIdent $$}
        
%%
{



}
