import Test.Hspec
import Music
import qualified Euterpea as E
import Data.Map as Map
import Control.Exception (evaluate)

main = hspec $ do
  describe "Music.mergeCoords" $ do
    it "merges coordinates" $
      mergeCoords (Coord 2 4 $ v ["S", "A"])
                  (Coord 1 2 $ v ["S"]) `shouldBe` (Coord 3 4 $ v ["S"])

    it "errors when range is beyond end of parent" $
      evaluate (mergeCoords (Coord 2 4 $ v ["S", "A"]) 
                            (Coord 1 3 $ v ["S"])) `shouldThrow` anyException

    it "errors when $ v are not a subset of parent" $
      evaluate (mergeCoords (Coord 2 4 $ v ["S", "A"]) (Coord 1 2 $ v ["T"]))
        `shouldThrow` anyException

    it "inherits parent when $ v are empty" $
      mergeCoords (Coord 2 4 $ v ["S", "A"]) 
                  (Coord 1 2 $ v []) `shouldBe` (Coord 3 4 $ v ["S", "A"])

  describe "Music.topExprToComp" $ do
    it "translates an empty expression" $
      topExprToComp (TopExpr []) `shouldBe` emptyComp
   
    it "translates an expression" $
      let example = TopExpr [((Coord 0 4 $ v ["S"]), [Expr $ Note E.A 4])] in
        topExprToComp example `shouldBe`
          comp [((Coord 0 4 $ v ["S"]), [Note E.A 4])]
    
    it "translates a nested expression" $
      let example =
            TopExpr [((Coord 4 8 $ v ["S", "A"]), 
                     [Expr $ Interval 4,
                      In ((Coord 0 2 $ v ["S"]), [Expr $ Note E.Fs 4]),
                      In ((Coord 2 4 $ v ["A"]), [Expr $ Note E.D 4])])] in
        topExprToComp example `shouldBe`
          comp [((Coord 4 8 $ v ["S", "A"]), [Interval 4]), 
                ((Coord 4 6 $ v ["S"]), [Note E.Fs 4]), 
                ((Coord 6 8 $ v ["A"]), [Note E.D 4])]

    it "gets ordering right" $
      let example =
            TopExpr [((Coord 4 6 $ v ["S"]), [Expr $ Note E.C 4]),
                     ((Coord 4 8 $ v ["S", "A"]), 
                     [Expr $ Interval 4,
                      In ((Coord 0 2 $ v ["S"]), [Expr $ Note E.Fs 4]),
                      In ((Coord 2 4 $ v ["A"]), [Expr $ Note E.D 4])])] in
        topExprToComp example `shouldBe`
          comp [((Coord 4 6 $ v ["S"]), [Note E.C 4]), 
                ((Coord 4 8 $ v ["S", "A"]), [Interval 4]), 
                ((Coord 4 6 $ v ["S"]), [Note E.Fs 4]), 
                ((Coord 6 8 $ v ["A"]), [Note E.D 4])]

    it "handles precompiled expressions" $
      let example =
            TopExpr [((Coord 4 6 $ v ["S"]), [Expr $ Note E.C 4]),
                     ((Coord 4 8 $ v ["S", "A"]), 
                      [Expr $ Interval 4,
                       In ((Coord 0 2 $ v ["S"]), [Expr $ Note E.Fs 4]),
                       In ((Coord 2 4 $ v ["A"]), 
                           [EComp $ comp [((Coord 0 1 $ v []), [Note E.D 3]), 
                                          ((Coord 1 2 $ v []), [Note E.E 3])]])])] in
        topExprToComp example `shouldBe`
          comp [((Coord 4 6 $ v ["S"]), [Note E.C 4]), 
                ((Coord 4 8 $ v ["S", "A"]), [Interval 4]), 
                ((Coord 4 6 $ v ["S"]), [Note E.Fs 4]), 
                ((Coord 6 7 $ v ["A"]), [Note E.D 3]),
                ((Coord 7 8 $ v ["A"]), [Note E.E 3])]
      

  describe "Music.compToEuterpea" $ do

    it "converts an empty comp" $
      compToEuterpea (comp []) == E.rest 0 

    it "converts a single line" $
      compToEuterpea (comp [((Coord 0 2 $ v ["A"]), [Note E.D 3]), 
                            ((Coord 2 4 $ v ["A"]), [Note E.E 3]),
                            ((Coord 4 8 $ v ["A"]), [Note E.Fs 3])])
      `shouldBe`
      E.note E.hn (E.D, 3) E.:+: E.note E.hn (E.E, 3) E.:+: E.note E.wn (E.Fs, 3)

    it "inserts rests" $
      compToEuterpea (comp [((Coord 0 2 $ v ["A"]), [Note E.D 3]), 
                            ((Coord 4 6 $ v ["A"]), [Note E.E 3]),
                            ((Coord 8 12 $ v ["A"]), [Note E.Fs 3])])
      `shouldBe`
      E.note E.hn (E.D, 3) E.:+: E.rest E.hn E.:+: 
      E.note E.hn (E.E, 3) E.:+: E.rest E.hn E.:+: 
      E.note E.wn (E.Fs, 3)
    it "handles multiple $ v " $
      compToEuterpea (comp [((Coord 0 2 $ v ["A"]), [Note E.D 3]), 
                            ((Coord 4 6 $ v ["A"]), [Note E.E 3]),
                            ((Coord 8 12 $ v ["A"]), [Note E.Fs 3]), 
                            ((Coord 0 2 $ v ["S"]), [Note E.D 4]), 
                            ((Coord 4 6 $ v ["S"]), [Note E.E 4]),
                            ((Coord 8 12 $ v ["S"]), [Note E.Fs 4])])
      `shouldBe`
      (E.note E.hn (E.D, 3) E.:+: E.rest E.hn E.:+: 
       E.note E.hn (E.E, 3) E.:+: E.rest E.hn E.:+: 
       E.note E.wn (E.Fs, 3)) E.:=:
      (E.note E.hn (E.D, 4) E.:+: E.rest E.hn E.:+: 
       E.note E.hn (E.E, 4) E.:+: E.rest E.hn E.:+: 
       E.note E.wn (E.Fs, 4))

