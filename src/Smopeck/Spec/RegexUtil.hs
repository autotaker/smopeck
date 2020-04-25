{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Spec.RegexUtil where

import           Data.Coerce
import           Data.List
import           Data.String
import           Text.Regex.TDFA

-- regex string
newtype RString = RString String deriving(Eq,Ord,Show)
instance IsString RString where
    fromString = RString

joinR :: [RString] -> RString
joinR = RString . intercalate "|" . coerce

compileR :: RString -> Regex
compileR (RString s) = makeRegexOpts blankCompOpt blankExecOpt s

matchR :: String -> RString -> Bool
matchR x (RString y) = x =~ ("^" ++ y ++ "$")

escapeR :: String -> RString
escapeR = RString . go
    where
    specials :: String
    specials = "^.[$()|*+?{\\"
    go [] = []
    go (c:cs)
        | c `elem` specials = '\\':c: go cs
        | otherwise = c : go cs
