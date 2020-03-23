import Test.Hspec
import Music
import qualified Euterpea as E
import Data.Map as Map
import Control.Exception (evaluate)

main = hspec $ do
  describe "Music.mergeCoords" $ do
    it "merges coordinates" $
      mergeCoords (2, 4, voices ["S", "A"]) (1, 2, voices ["S"]) `shouldBe` (3, 4, voices ["S"])

    it "errors when range is beyond end of parent" $
      evaluate (mergeCoords (2, 4, voices ["S", "A"]) (1, 3, voices ["S"])) `shouldThrow` anyException

    it "errors when voices are not a subset of parent" $
      evaluate (mergeCoords (2, 4, voices ["S", "A"]) (1, 2, voices ["T"]))
        `shouldThrow` anyException

  describe "Music.topExprToComp" $ do
    it "translates an empty expression" $
      topExprToComp (TopExpr []) `shouldBe` emptyComp
   
    it "translates an expression" $
      let example = TopExpr [((0, 4, voices ["S"]), [Expr $ Note E.A 4])] in
        topExprToComp example `shouldBe`
          comp [((0, 4, voices ["S"]), [Note E.A 4])]
    
    it "translates a nested expression" $
      let example =
            TopExpr [((4, 8, voices ["S", "A"]), 
                     [Expr $ Interval 4,
                      In ((0, 2, voices ["S"]), [Expr $ Note E.Fs 4]),
                      In ((2, 4, voices ["A"]), [Expr $ Note E.D 4])])] in
        topExprToComp example `shouldBe`
          comp [((4, 8, voices ["S", "A"]), [Interval 4]), 
                ((4, 6, voices ["S"]), [Note E.Fs 4]), 
                ((6, 8, voices ["A"]), [Note E.D 4])]

    it "gets ordering right" $
      let example =
            TopExpr [((4, 6, voices ["S"]), [Expr $ Note E.C 4]),
                     ((4, 8, voices ["S", "A"]), 
                     [Expr $ Interval 4,
                      In ((0, 2, voices ["S"]), [Expr $ Note E.Fs 4]),
                      In ((2, 4, voices ["A"]), [Expr $ Note E.D 4])])] in
        topExprToComp example `shouldBe`
          comp [((4, 6, voices ["S"]), [Note E.C 4]), 
                ((4, 8, voices ["S", "A"]), [Interval 4]), 
                ((4, 6, voices ["S"]), [Note E.Fs 4]), 
                ((6, 8, voices ["A"]), [Note E.D 4])]

  describe "Music.compToEuterpea" $ do

    it "converts an empty comp" $
      compToEuterpea (comp []) == E.rest 0 

    it "converts a single line" $
      compToEuterpea (comp [((0, 2, voices ["A"]), [Note E.D 3]), 
                            ((2, 4, voices ["A"]), [Note E.E 3]),
                            ((4, 8, voices ["A"]), [Note E.Fs 3])])
      `shouldBe`
      E.note E.hn (E.D, 3) E.:+: E.note E.hn (E.E, 3) E.:+: E.note E.wn (E.Fs, 3)

    it "inserts rests" $
      compToEuterpea (comp [((0, 2, voices ["A"]), [Note E.D 3]), 
                            ((4, 6, voices ["A"]), [Note E.E 3]),
                            ((8, 12, voices ["A"]), [Note E.Fs 3])])
      `shouldBe`
      E.note E.hn (E.D, 3) E.:+: E.rest E.hn E.:+: 
      E.note E.hn (E.E, 3) E.:+: E.rest E.hn E.:+: 
      E.note E.wn (E.Fs, 3)
    it "handles multiple voices " $
      compToEuterpea (comp [((0, 2, voices ["A"]), [Note E.D 3]), 
                            ((4, 6, voices ["A"]), [Note E.E 3]),
                            ((8, 12, voices ["A"]), [Note E.Fs 3]), 
                            ((0, 2, voices ["S"]), [Note E.D 4]), 
                            ((4, 6, voices ["S"]), [Note E.E 4]),
                            ((8, 12, voices ["S"]), [Note E.Fs 4])])
      `shouldBe`
      (E.note E.hn (E.D, 3) E.:+: E.rest E.hn E.:+: 
       E.note E.hn (E.E, 3) E.:+: E.rest E.hn E.:+: 
       E.note E.wn (E.Fs, 3)) E.:=:
      (E.note E.hn (E.D, 4) E.:+: E.rest E.hn E.:+: 
       E.note E.hn (E.E, 4) E.:+: E.rest E.hn E.:+: 
       E.note E.wn (E.Fs, 4))

