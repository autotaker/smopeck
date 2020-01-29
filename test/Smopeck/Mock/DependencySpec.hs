module Smopeck.Mock.DependencySpec where

import qualified Data.Set                as S
import           Smopeck.Mock.Dependency
import           Smopeck.Mock.Location
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Mock.Dependency.updateDepGraph" $ do
        let vertices = S.fromList [
                Root "it",
                Root "it" `Field` "field1",
                Root "it" `Field` "field2",
                Root "it" `Field` "field1" `Get` 0,
                Root "it" `Field` "field1" `Get` 1,
                Root "it" `Field` "field1" `Field`  "length"
                ]
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
