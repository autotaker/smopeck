module Smopeck.Mock.LocationSpec where

import           Smopeck.Mock.Location
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Mock.Location.match" $ do
        it "return true if lhs is the same as rhs" $ do
            let lhs = Root "root" `Field` "field1" `Get` (BlobInt 1)
                conv (BlobInt x) = x
                conv BlobAny     = 0
                rhs = fmap conv lhs
            match lhs rhs `shouldBe` True
        it "return true if lhs is the any blob of rhs" $ do
            let rhs = Root "root" `Field` "field1" `Get` 1
                lhs = fmap (const BlobAny) rhs
            match lhs rhs `shouldBe` True
        it "return false if the shape differs" $ do
            let rhs = Root "root" `Field` "field1" `Get` 1
                lhs = Root "root" `Get` (BlobInt 1) `Field` "field1"
            match lhs rhs `shouldBe` False
        it "return false if field differs" $ do
            let rhs = Root "root" `Field` "field1" `Get` 1
                lhs = Root "root" `Field` "field2" `Get` (BlobInt 1)
            match lhs rhs `shouldBe` False
        it "return false if index differs" $ do
            let rhs = Root "root" `Field` "field1" `Get` 1
                lhs = Root "root" `Field` "field1" `Get` (BlobInt 2)
            match lhs rhs `shouldBe` False
        it "return false if root differs" $ do
            let rhs = Root "root1" `Field` "field1" `Get` 1
                lhs = Root "root2" `Field` "field1" `Get` (BlobInt 1)
            match lhs rhs `shouldBe` False

