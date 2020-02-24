{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module Smopeck.Logic.Equality where

import           Control.Monad
import           Data.Char
import qualified Data.Set            as S
import           Smopeck.Logic.Model
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp    hiding (interpret)
import           System.Random.MWC

newtype Equality a = Equality a

instance Model (Equality Bool) where
    newtype Atom (Equality Bool) = BOneOf (S.Set Bool)
    top = BOneOf (S.fromList [True, False])
    bot = BOneOf S.empty
    join (BOneOf xs) (BOneOf ys) = BOneOf (S.union xs ys)
    meet (BOneOf xs) (BOneOf ys) = BOneOf (S.intersection xs ys)
    generate st (BOneOf xs)
        | S.null xs = pure Nothing
        | otherwise = do
            i <- uniformR (0,S.size xs - 1) st
            pure $ Just $! Equality $ S.elemAt i xs
    interpret Eq (LBool v) = BOneOf (S.singleton v)
    interpret op v         = error $ "cannot interpret: " ++ show (op, v)

instance Model (Equality String) where
    data Atom (Equality String) = SAny | SOneOf !(S.Set String)
    top = SAny
    bot = SOneOf S.empty
    join SAny _                  = SAny
    join _ SAny                  = SAny
    join (SOneOf xs) (SOneOf ys) = SOneOf (S.union xs ys)
    meet SAny ys                 = ys
    meet xs SAny                 = xs
    meet (SOneOf xs) (SOneOf ys) = SOneOf (S.intersection xs ys)
    generate st SAny = do
        n <- uniformR (0, 30) st
        str <- replicateM n $ chr <$> uniformR (32, 126) st
        pure $ Just $! Equality str
    generate st (SOneOf xs)
        | S.null xs = pure Nothing
        | otherwise = do
            i <- uniformR (0,S.size xs - 1) st
            pure $ Just $! Equality $ S.elemAt i xs
    interpret Eq (LString v) = SOneOf (S.singleton v)
    interpret op v           = error $ "cannot interpret: " ++ show (op, v)
