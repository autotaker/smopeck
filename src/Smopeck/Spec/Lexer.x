{
module Smopeck.Spec.Lexer where
}

%wrapper "monadUserState"

$capital = [A-Z]
$small = [a-z]
$alphanum = [A-Za-z0-9]

tokens :-
  <0> $white+                               ;
  <0> ^"type"                               { token $ \_ _ -> Type }
  <0> ^"endpoint"                           { token $ \_ _ -> Endpoint }
  <0> $capital $alphanum*                   { token $ \s len -> UpperId $ lexeme s len }
  <0> $small $alphanum*                     { token $ \s len -> LowerId $ lexeme s len }
  <0> "|"                                   { token $ \_ _ -> Join }
  <0> "&"                                   { token $ \_ _ -> Meet }
  <0> "="                                   { token $ \_ _ -> Eq }
  <0> "<"                                   { token $ \_ _ -> Lt }
  <0> ">"                                   { token $ \_ _ -> Gt }
  <0> "("                                   { token $ \_ _ -> Lpar }
  <0> ")"                                   { token $ \_ _ -> Rpar }
  <0> "{"                                   { token $ \_ _ -> Lbra }
  <0> "}"                                   { token $ \_ _ -> Rbra }
  <0> "["                                   { token $ \_ _ -> Lsq }
  <0> "]"                                   { token $ \_ _ -> Rsq }
  <0> ":"                                   { token $ \_ _ -> Colon }
  <0> ","                                   { token $ \_ _ -> Comma }
  <0> \.                                    { token $ \_ _ -> Dot }
  <0> \#                                    { token $ \_ _ -> Hash }
  <0> \"                                    { begin dqstr }
  <dqstr> \\[\" \\ n r t]                   { \s len -> pushChar (unescape $ lexeme s len) >> alexMonadScan }
  <dqstr> \"                                { (\_ _ -> DQString <$> flushStringValue) `andBegin` 0 }
  <dqstr> [^"]                              { \s len -> let [ch] = lexeme s len in pushChar ch >> alexMonadScan }

{
data Token = 
    Type
  | Endpoint
  | Eq | Join | Meet
  | Lpar | Rpar
  | Lbra | Rbra
  | Lt | Gt
  | Lsq | Rsq
  | Colon
  | Comma
  | Hash | Dot
  | DQString String
  | SQString String
  | Regex String
  | Number Double
  | LowerId String
  | UpperId String
  | EOF
  deriving(Eq,Ord,Show)

data AlexUserState = AlexUserState {
    lexerStringBuffer :: String
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerStringBuffer = [] }

flushStringValue :: Alex String
flushStringValue = do
    ust <- alexGetUserState
    alexSetUserState ust{ lexerStringBuffer = []}
    pure $ reverse $ lexerStringBuffer ust

pushChar :: Char -> Alex ()
pushChar ch = do
    ust <- alexGetUserState
    alexSetUserState ust{ lexerStringBuffer = ch : lexerStringBuffer ust }


lexeme :: AlexInput -> Int -> String
lexeme (_,_, _, s) len = take len s

unescape :: String -> Char
unescape ['\\', 'n'] = '\n'
unescape ['\\', 'r'] = '\r'
unescape ['\\', 't'] = '\t'
unescape ['\\', ch] = ch
unescape str = error $ "cannot unescape: " ++ show str
-- Footer

alexEOF :: Alex Token
alexEOF = pure EOF
}