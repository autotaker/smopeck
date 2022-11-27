module Smopeck.Mock.Value where

import qualified Data.Map              as M
import           Data.Scientific
import qualified Data.Set              as S
import           Smopeck.Mock.Location
import           Smopeck.Spec.Syntax

data Value =
    VNull
    | VUndefined
    | VBool Bool
    | VNumber Scientific
    | VString String
    | VObject (S.Set FieldName)
    | VArray
    deriving(Show)

type Assignment = M.Map Location Value
