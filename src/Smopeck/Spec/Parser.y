{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Spec.Parser(
    runLexer
    , runLexerMock
    , parse
    , parseExpr
    , parseTypeExp
) where
import Smopeck.Spec.Lexer hiding (Eq, Lt, Gt, Add, Sub, Mul, Div, Mod, Lte, Gte, Match, And,Or)
import qualified Smopeck.Spec.Lexer as L

import Smopeck.Spec.Syntax
import qualified Smopeck.Spec.TypeExp as T
import Control.Monad.Free
}

%name parse TopLevelDefList
%name parseTypeExp TypeExp
%partial parseExpr Exp
%tokentype { Token }
%lexer {lexerWrap} { EOF }
%monad { Free Lexer }  
%error { parseError }

%token
    type     { Type }
    endpoint { Endpoint }
    true     { TTrue }
    false    { TFalse }
    '&&'     { L.And }
    '||'     { L.Or }
    '='      { L.Eq }
    '=~'     { L.Match }
    '<'      { L.Lt }
    '>'      { L.Gt }
    '<='     { L.Lte }
    '>='     { L.Gte }
    '+'      { L.Add }
    '-'      { L.Sub }
    '*'      { L.Mul }
    '/'      { L.Div }
    '%'      { L.Mod }
    '|'      { Join }
    '&'      { Meet }
    ','      { Comma }
    '.'      { Dot }
    ':'      { Colon }
    '{'      { Lbra }
    '}'      { Rbra }
    '['      { Lsq }
    ']'      { Rsq }
    '('      { Lpar }
    ')'      { Rpar }
    '@'      { As }
    '?'      { Cond }
    lower    { LowerId $$ }
    upper    { UpperId $$ }
    number   { Number $$ }
    dqLiteral { DQString  $$ }
    sqLiteral { SQString  $$ }
    sqRegex  { SQRegex $$ }

%left '?'
%left '|' '||'
%left '&' '&&'
%nonassoc '=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left NEG

%%

TopLevelDefList 
    : TopLevelDef                 { [$1] }
    | TopLevelDef TopLevelDefList { $1 : $2 } 

TopLevelDef : TypeDef       { $1 }
            | EndpointDef   { $1 }


TypeDef : type upper '=' TypeExp { TypeDef $2 $4 }

TypeExpName : upper { fTypeExp $1 }
TypeExpNameBind : TypeExpName { $1 "." }
                | TypeExpName '@' lower { $1 $3 }
TypeExpNameBindExt
    : TypeExpNameBind { $1 [] }                
    | TypeExpNameBind TypeExtension { $1 $2 }                
TypeExpNameBindExtRef
    : TypeExpNameBindExt { $1 [] }
    | TypeExpNameBindExt TypeRef { $1 $2 }

TypeExp : TypeExpNameBindExtRef { $1 }
        | Literal { LElem (T.LiteralType $1) }
        | '(' TypeExp ')' { $2 }
        | TypeExp '&' TypeExp { LMeet $1 $3 }
        | TypeExp '|' TypeExp { LJoin $1 $3 }
        | TypeExp '?' Exp     { LExt $ T.HasCondF $3 $1 }

TypeExtension 
    : '{' '}'                   { [] } 
    | '{' TypeExtensionList '}' { $2 }
TypeExtensionList 
    : TypeBinding                       { [$1]    }
    | TypeBinding ',' TypeExtensionList { $1 : $3 }
TypeBinding : Field ':' TypeExp { ($1, $3) }

TypeRef
    : '[' ']' { [] }
    | '[' TypeRefList ']' { $2 }
TypeRefList 
    : TypeRefEntry                 { [$1]    }
    | TypeRefEntry ',' TypeRefList { $1 : $3 }

TypeRefEntry : '.' CompOp Exp { ($2, $3) }

CompOp : '=' { Eq }
       | '<' { Lt }
       | '>' { Gt }
       | '<=' { Lte }
       | '>=' { Gte }
       | '=~' { Match }

Field : lower               { FieldString $1 }
      | upper               { FieldString $1 }
      | sqLiteral           { FieldString $1 }
      | lower '(' lower ')' { FieldIndex (BindName $3) }
FieldExp : lower             { FieldString $1 }
         | lower '(' Exp ')' { FieldIndex $3 }

EndpointDef : endpoint dqLiteral upper TypeExtension { EndpointDef $2 $3 $4 } 

LocationExp : '.' { Root (Relative 0) }
            | lower { Root (Absolute $1) }
            | LocationExp '.' FieldExp { Chain $1 $3 }

Exp : ExpF { T.Exp $1 }
ExpF 
    : LocationExp   { Var $1 }
    | lower '(' ArgList ')' { App (Func $1) $3 }
    | Literal       { Literal $1 }
    | '(' ExpF ')'  { $2 }
    | ExpF '||' ExpF { App Or [$1, $3] }
    | ExpF '&&' ExpF { App And [$1, $3] }
    | ExpF '=' ExpF { App Eq [$1, $3] }
    | ExpF '<' ExpF { App Lt [$1, $3] }
    | ExpF '>' ExpF { App Gt [$1, $3] }
    | ExpF '<=' ExpF { App Lte [$1, $3] }
    | ExpF '>=' ExpF { App Gte [$1, $3] }
    | ExpF '+' ExpF { App Add [$1, $3] }
    | ExpF '-' ExpF { App Sub [$1, $3] }
    | ExpF '*' ExpF { App Mul [$1, $3] }
    | ExpF '/' ExpF { App Div [$1, $3] }
    | ExpF '%' ExpF { App Mod [$1, $3] }
    | '-' ExpF %prec NEG { App Sub [$2] }
Literal : dqLiteral { LDQString $1 }
        | sqLiteral { LString $1 }
        | sqRegex   { LRegex $1 }
        | number    { LNumber $1 }
        | true      { LBool True }
        | false     { LBool False }
ArgList 
    : ExpF  { [$1] }
    | ExpF ',' ArgList { $1 : $3 }

{
-- Footer

data Lexer a = Lex (Token -> a) | Error String | GetPos (AlexPosn -> a)
    deriving(Functor)

runLexer :: Free Lexer a -> Alex a
runLexer (Pure a) = pure a
runLexer (Free (Lex f)) = alexMonadScan >>= runLexer . f
runLexer (Free (GetPos f)) = do
    (pos,_,_,_) <- alexGetInput
    runLexer $ f pos
runLexer (Free (Error err)) = alexError err

runLexerMock :: Free Lexer a -> [Token] -> Either String a
runLexerMock (Pure a) _ = pure a
runLexerMock (Free (Lex f)) (token:ts) = runLexerMock (f token) ts
runLexerMock (Free (Lex f)) [] = runLexerMock (f EOF) []
runLexerMock (Free (GetPos f)) ts = runLexerMock (f alexStartPos) ts
runLexerMock (Free (Error str)) _ = Left str

lexerWrap :: (Token -> Free Lexer a) -> Free Lexer a
lexerWrap f = Free (Lex f)

parseError :: Token -> Free Lexer a 
parseError token = 
    Free $ GetPos $ \pos ->
        Free (Error ("unexpected token:" ++ show token ++ " at " ++ show pos))

}