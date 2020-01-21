{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Spec.Parser(
    runLexer
    , runLexerMock
    , parse
) where
import Smopeck.Spec.Lexer
import Smopeck.Spec.Syntax
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
    '='      { Eq }
    '<'      { Lt }
    '>'      { Gt }
    var      { Var $$ }
    '|'      { Join }
    '&'      { Meet }
    ','      { Comma }
    ':'      { Colon }
    '{'      { Lbra }
    '}'      { Rbra }
    typeName { TyName $$ }
    dqLiteral { DQString  $$ }

%%

TopLevelDef : TypeDef       { $1 }
            | EndpointDef   { $1 }


TypeDef : type typeName '=' TypeExp { TypeDef $2 $4 }

TypeExp : typeName                  { TypeExp $1 [] [] }
TypeExtension 
    : '{' '}'                   { [] } 
    | '{' TypeExtensionList '}' { $2 }
TypeExtensionList : TypeBinding ',' TypeExtensionList { $1 : $3 }
TypeBinding : var ':' TypeExp { ($1, $3) }

EndpointDef : endpoint dqLiteral typeName TypeExtension { EndpointDef $2 $3 $4 } 

{
-- Footer
parseError :: Token -> Free Lexer a 
parseError token = Free (Error ("unexpected token:" ++ show token))

data Mode = Default | Mock

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


}