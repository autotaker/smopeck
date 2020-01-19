{
module Smopeck.Spec.Lexer where
}

%wrapper "posn"

$capital = [A-Z]
$small = [a-z]
$alphanum = [A-Za-z0-9]

tokens :-
  $white+                               ;
  "type"                                { \_ _ -> Type }
  "endpoint"                            { \_ _ -> Endpoint }
  $capital $alphanum*                   { \_ s -> TyName s }
  $small $alphanum*                     { \_ s -> Var s }
  "|"                                   { \_ _ -> Join }
  "&"                                   { \_ _ -> Meet }
  "="                                   { \_ _ -> Eq }
  "<"                                   { \_ _ -> Lt }
  ">"                                   { \_ _ -> Gt }
  "("                                   { \_ _ -> Lpar }
  ")"                                   { \_ _ -> Rpar }
  "{"                                   { \_ _ -> Lbra }
  "}"                                   { \_ _ -> Rbra }
  "["                                   { \_ _ -> Lsq }
  "]"                                   { \_ _ -> Rsq }
  ":"                                   { \_ _ -> Colon }
  ","                                   { \_ _ -> Comma }

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