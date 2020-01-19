{
module Smopeck.Spec.Lexer where
}

%wrapper "basic"

$capital = [A-Z]
$small = [a-z]
$alphanum = [A-Za-z0-9]

tokens :-
  "type"                                { \_ -> Type }
  "endpoint"                            { \_ -> Endpoint }
  $capital $alphanum*                   { \s -> TyName s }
  $small $alphanum*                     { \s -> Var s }
  "|"                                   { \_ -> Join }
  "&"                                   { \_ -> Meet }
  "="                                   { \_ -> Eq }
  "<"                                   { \_ -> Lt }
  ">"                                   { \_ -> Gt }
  "("                                   { \_ -> Lpar }
  ")"                                   { \_ -> Rpar }
  "{"                                   { \_ -> Lbra }
  "}"                                   { \_ -> Rbra }
  "["                                   { \_ -> Lsq }
  "]"                                   { \_ -> Rsq }
  ":"                                   { \_ -> Colon }
  ","                                   { \_ -> Comma }

{
data Token = 
    Type
  | Eq
  | Join
  | Meet
  | Lpar
  | Rpar
  | Lbra
  | Rbra
  | Lt
  | Gt
  | Lsq
  | Rsq
  | Colon
  | Comma
  | DQString String
  | SQString String
  | Regex String
  | Number Double
  | Var String
  | TyName String
  | Accessor (Maybe String) String
  | Endpoint
  deriving(Eq,Ord,Show)


-- Footer
}