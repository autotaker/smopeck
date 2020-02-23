module Smopeck.Mock.ConstraintSpec where


import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.HashTable.IO       as H
import qualified Data.Map                as M
import           Smopeck.Mock.Constraint
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Mock.Constraint.evalL" $ do
        it "gen number" $ do
            let ty = LElem (TypeExpF (Prim PNumber) "." M.empty [])
            v <- mockJson M.empty ty
            print v
        it "gen number in range" $ do
            let ty = LElem (TypeExpF (Prim PNumber) "." M.empty [(Root (), Eq, Exp $ Literal (LNumber 0))])
            v <- mockJson M.empty ty
            v `shouldBe` toJSON (0 :: Int)
            print v
        it "gen object" $ do
            let tyInt = LElem (TypeExpF (Prim PInt) "." M.empty [])
                ty = LElem (TypeExpF (Prim PObject) "." (M.fromList [("hoge", tyInt), ("fuga", tyInt)])[])
                ty2 = LElem (TypeExpF (Prim PArray) "." (M.fromList [("length", tyInt), ("get", ty)]) [])
            v <- mockJson M.empty ty2
            print $ v
