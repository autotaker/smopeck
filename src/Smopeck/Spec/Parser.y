{
module Smopeck.Spec.Parser(parseDef) where
import Smopeck.Spec.Lexer
import Smopeck.Spec.Syntax
}

%name parse
%tokentype { Token }

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
happyError :: [Token] -> a
happyError tokens = error $ "parse error: " ++ show tokens 

parseDef :: [Token] -> TopLevelDef
parseDef tokens = parse tokens 
}