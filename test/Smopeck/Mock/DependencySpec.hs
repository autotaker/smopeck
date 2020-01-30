module Smopeck.Mock.DependencySpec where

import           Control.Monad
import qualified Data.Set                as S
import           Smopeck.Mock.Dependency
import           Smopeck.Mock.Location
import           Test.Hspec

spec :: Spec
spec = do
    let vertices = S.fromList [
            Root "it",
            Root "it" `Field` "field1",
            Root "it" `Field` "field2",
            Root "it" `Field` "field1" `Get` 0,
            Root "it" `Field` "field1" `Get` 1,
            Root "it" `Field` "field1" `Field`  "length"
            ]
    describe "Smopeck.Mock.Dependency.updateDepGraph" $ do
        it "normal edge" $ do
            let edges = S.fromList []
                depEdges = [ DepEdge (Root "it" `Field` "field1") (Root "it" `Field` "field2") ]
                edges' = S.fromList [
                    (Root "it" `Field` "field1", Root "it" `Field` "field2")
                    ]
            updateDepGraph (vertices, edges) depEdges `shouldBe` (vertices, edges')
        it "blob edge" $ do
            let edges = S.fromList []
                depEdges = [ DepEdge (Root "it" `Field` "field2") (Root "it" `Field` "field1" `Get` BlobAny) ]
                edges' = S.fromList [
                    (Root "it" `Field` "field2", Root "it" `Field` "field1" `Get` 0),
                    (Root "it" `Field` "field2", Root "it" `Field` "field1" `Get` 1)
                    ]
            updateDepGraph (vertices, edges) depEdges `shouldBe` (vertices, edges')
    describe "Smopeck.Mock.Dependency.findDepFreeLocation" $
        it "returns dependency free vertex" $ do
            let edges :: S.Set (Location, Location)
                edges = S.fromList [
                    (Root "it" `Field` "field1", Root "it" `Field` "field2")
                    ]
                assigned, expectedSet :: S.Set Location
                assigned = S.empty
                expectedSet = S.fromList [
                    Root "it",
                    Root "it" `Field` "field1",
                    Root "it" `Field` "field1" `Get` 0,
                    Root "it" `Field` "field1" `Get` 1,
                    Root "it" `Field` "field1" `Field`  "length"
                    ]
            (findDepFreeLocation (vertices, edges) assigned >>=
                \v -> guard $ S.member v expectedSet) `shouldBe` Just ()




