{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Spec.Parser(
    runLexer
    , runLexerMock
    , parse
) where
import Smopeck.Spec.Lexer hiding (Eq, Lt, Gt)
import qualified Smopeck.Spec.Lexer as L

import Smopeck.Spec.Syntax
import qualified Smopeck.Spec.TypeExp as T
import Control.Monad.Free
}

%name parse
%tokentype { Token }
%lexer {lexerWrap} { EOF }
%monad { Free Lexer }  
%error { parseError }

%token
    type     { Type }
    endpoint { Endpoint }
    '='      { L.Eq }
    '<'      { L.Lt }
    '>'      { L.Gt }
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
    lower    { LowerId $$ }
    upper    { UpperId $$ }
    dqLiteral { DQString  $$ }

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

TypeRefEntry : '.' '=' Exp { (Eq, $3) }

Field : lower               { FieldString $1 }
      | lower '(' lower ')' { FieldIndex (BindName $3) }
FieldExp : lower             { FieldString $1 }
         | lower '(' Exp ')' { FieldIndex $3 }

EndpointDef : endpoint dqLiteral upper TypeExtension { EndpointDef $2 $3 $4 } 

LocationExp : '.' { Root (Relative 0) }
            | lower { Root (Absolute $1) }
            | LocationExp '.' FieldExp { Chain $1 $3 }

Exp : LocationExp { T.Exp (Var $1) }
    | Exp 

{
-- Footer

data Lexer a = Lex (Token -> a) | Error String
    deriving(Functor)

runLexer :: Free Lexer a -> Alex a
runLexer (Pure a) = pure a
runLexer (Free (Lex f)) = alexMonadScan >>= runLexer . f
runLexer (Free (Error err)) = alexError err

runLexerMock :: Free Lexer a -> [Token] -> Either String a
runLexerMock (Pure a) _ = pure a
runLexerMock (Free (Lex f)) (token:ts) = runLexerMock (f token) ts
runLexerMock (Free (Lex f)) [] = runLexerMock (f EOF) []
runLexerMock (Free (Error str)) _ = Left str

lexerWrap :: (Token -> Free Lexer a) -> Free Lexer a
lexerWrap f = Free (Lex f)

parseError :: Token -> Free Lexer a 
parseError token = Free (Error ("unexpected token:" ++ show token))

}