module Smopeck.Logic.NumberSpec where

import           Smopeck.Logic.Number
import           Test.Hspec

spec :: Spec
spec = do
    describe "Smopeck.Logic.Number.compare(LeftEnd)" $ do
        it "compare Open Open == EQ" $
            compare (LeftEnd Open :: LeftEnd (End ())) (LeftEnd Open) `shouldBe` EQ
        it "compare Open (Inclusive ()) == LT" $
            compare (LeftEnd Open) (LeftEnd (Inclusive ())) `shouldBe` LT
        it "compare (Inclusive ()) (Inclusive ()) == EQ" $
            compare (LeftEnd (Inclusive ())) (LeftEnd (Inclusive ())) `shouldBe` EQ
        it "compare (Exclusive ()) (Exclusive ()) == EQ" $
            compare (LeftEnd (Exclusive ())) (LeftEnd (Exclusive ())) `shouldBe` EQ
        it "compare (Inclusive ()) (Exclusive ()) == LT" $
            compare (LeftEnd (Inclusive ())) (LeftEnd (Exclusive ())) `shouldBe` LT
        it "compare (Inclusive 1) (Exclusive 0) == GT" $
            compare (LeftEnd (Inclusive 1 :: End Int)) (LeftEnd (Exclusive 0)) `shouldBe` GT
        it "compare (Exclusive 0) (Inclusive 1) == LT" $
            compare (LeftEnd (Exclusive 0 :: End Int)) (LeftEnd (Inclusive 1)) `shouldBe` LT
        it "compare (Exclusive 1) (Inclusive 0) == GT" $
            compare (LeftEnd (Exclusive 1 :: End Int)) (LeftEnd (Inclusive 0)) `shouldBe` GT
    describe "Smopeck.Logic.Number.compare(RightEnd)" $ do
        it "compare Open Open == EQ" $
            compare (RightEnd Open :: RightEnd (End ())) (RightEnd Open) `shouldBe` EQ
        it "compare Open (Inclusive ()) == GT" $
            compare (RightEnd Open) (RightEnd (Inclusive ())) `shouldBe` GT
        it "compare (Inclusive ()) Open == GT" $
            compare (RightEnd (Inclusive ())) (RightEnd Open) `shouldBe` LT
        it "compare (Inclusive ()) (Inclusive ()) == EQ" $
            compare (RightEnd (Inclusive ())) (RightEnd (Inclusive ())) `shouldBe` EQ
        it "compare (Exclusive ()) (Exclusive ()) == EQ" $
            compare (RightEnd (Exclusive ())) (RightEnd (Exclusive ())) `shouldBe` EQ
        it "compare (Inclusive ()) (Exclusive ()) == GT" $
            compare (RightEnd (Inclusive ())) (RightEnd (Exclusive ())) `shouldBe` GT
        it "compare (Inclusive 1) (Exclusive 0) == GT" $
            compare (RightEnd (Inclusive 1 :: End Int)) (RightEnd (Exclusive 0)) `shouldBe` GT
        it "compare (Exclusive 0) (Inclusive 1) == LT" $
            compare (RightEnd (Exclusive 0 :: End Int)) (RightEnd (Inclusive 1)) `shouldBe` LT
        it "compare (Exclusive 1) (Inclusive 0) == GT" $
            compare (RightEnd (Exclusive 1 :: End Int)) (RightEnd (Inclusive 0)) `shouldBe` GT

    describe "Smopeck.Logic.Number.joinArea" $ do
        it "joinArea [(-∞, 0]] [[3, 5)] == [(-∞, 0], [3, 5)]" $ do
            let range1, range2, range3 :: [Range Int]
                range1 = [Range (LeftEnd Open) (RightEnd (Inclusive 0)) ]
                range2 = [Range (LeftEnd (Inclusive 3)) (RightEnd (Exclusive 5)) ]
                range3 = range1 ++ range2
            joinArea range1 range2 `shouldBe` range3
        it "joinArea  [[3, 5)] [(-∞, 0]]== [(-∞, 0], [3, 5)]" $ do
            let range1, range2, range3 :: [Range Int]
                range1 = [Range (LeftEnd (Inclusive 3)) (RightEnd (Exclusive 5)) ]
                range2 = [Range (LeftEnd Open) (RightEnd (Inclusive 0)) ]
                range3 = range2 ++ range1
            joinArea range1 range2 `shouldBe` range3
        it "joinArea [(-∞, 1]] [[1, 5)] == [(-∞, 5)]" $ do
            let range1, range2, range3 :: [Range Int]
                range1 = [Range (LeftEnd Open) (RightEnd (Inclusive 1)) ]
                range2 = [Range (LeftEnd (Inclusive 1)) (RightEnd (Exclusive 5)) ]
                range3 = [Range (LeftEnd Open) (RightEnd (Exclusive 5))]
            joinArea range1 range2 `shouldBe` range3
        it "joinArea [(1, 5]] [[2, 3)] == [(1, 5]]" $ do
            let range1, range2, range3 :: [Range Int]
                range1 = [Range (LeftEnd (Exclusive 1)) (RightEnd (Inclusive 5)) ]
                range2 = [Range (LeftEnd (Inclusive 2)) (RightEnd (Exclusive 3)) ]
                range3 = [Range (LeftEnd (Exclusive 1)) (RightEnd (Inclusive 5))]
            joinArea range1 range2 `shouldBe` range3

