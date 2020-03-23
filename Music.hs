{-# LANGUAGE FlexibleInstances #-}

module Music 
  (Coord, 
   voices,
   In, 
   AtomicExpr(Note, Interval),
   Expr(In, Expr), 
   TopExpr(TopExpr), 
   Comp(Comp),
   comp,
   emptyComp,
   mergeCoords,
   exprToComp,
   topExprToComp,
   Score,
   compToScore,
   scoreToEuterpea,
   compToEuterpea) where

import qualified Data.Set as Set
import Data.Semigroup (sconcat)
import Data.List (sort, find)
import qualified Data.Map.Strict as Map
import Control.Monad.Writer
import Control.Exception
import qualified Euterpea as E 

type Coord = (Rational, Rational, Set.Set String)
voices :: [String] -> Set.Set String
voices = Set.fromList

type In = (Coord, [Expr])
data AtomicExpr = Note E.PitchClass Int 
                | Interval Int
  deriving (Eq, Show, Ord)

data Expr = In In | Expr AtomicExpr deriving (Eq, Show)
newtype TopExpr = TopExpr [In] deriving (Eq, Show)

data Comp = Comp [Coord] (Map.Map Coord [AtomicExpr]) deriving (Eq, Show)
emptyComp :: Comp
emptyComp = Comp [] Map.empty

type ScoreVoice = [((Rational, Rational), AtomicExpr)]
type Score = Map.Map String ScoreVoice

data MusicException = MusicException deriving (Show)
instance Exception MusicException

comp :: [(Coord, [AtomicExpr])] -> Comp
comp li = Comp (foldr go [] li) $ Map.fromListWith (++) $ reverse li
  where
    go :: (Coord, [AtomicExpr]) -> [Coord] -> [Coord]
    go (coord, exprs) acc = (map (\_ -> coord) exprs) ++ acc

mergeCoords :: Coord -> Coord -> Coord
mergeCoords (ps, pe, pvs) (cs, ce, cvs) = 
  case () of _ 
              | end > pe -> throw MusicException
              | not (cvs `Set.isSubsetOf` pvs) -> throw MusicException
              | otherwise -> (cs + ps, end, cvs)
  where end = ce + ps

instance Semigroup Comp where 
  (<>) (Comp lOrder lMap) (Comp rOrder rMap) = 
    Comp (lOrder ++ rOrder) $ Map.unionWith (++) lMap rMap

instance Monoid Comp where mempty = emptyComp

exprToComp :: Comp -> Coord -> Expr -> Comp
exprToComp comp pc (In (c, exprs)) = 
  mconcat (map (exprToComp comp (mergeCoords pc c)) exprs)
exprToComp comp c (Expr e) = Comp [c] $ Map.fromListWith (++) [(c, [e])]

topExprToComp :: TopExpr -> Comp
topExprToComp (TopExpr ins) = 
  mconcat (map go ins) where 
    go (c, exprs) = mconcat (map (exprToComp emptyComp c) exprs)

compToScore :: Comp -> Score
compToScore (Comp _ comp) = 
  Map.map sort $ Map.foldrWithKey go Map.empty comp
  where
    selectNote (Note _ _) = True
    selectNote expr = False
    insertNote voice start end exprs acc = 
      case (find selectNote exprs) of
        Nothing -> acc
        Just note -> 
          let value = ((start, end), note) in
            if Map.member voice acc then 
              Map.adjust ((:) value) voice acc
            else Map.insert voice [value] acc
    go :: Coord -> [AtomicExpr] -> Score -> Score
    go (s, e, vs) exprs acc = 
      if (length vs == 1) then insertNote (Set.elemAt 0 vs) s e exprs acc else acc

-- attribution: my better half
scoreToEuterpea :: Score -> E.Music E.Pitch
scoreToEuterpea = foldr (\ notes m -> mkNotes notes `horz` m) (E.rest 0) where
  horz a (E.Prim (E.Rest 0)) = a
  horz (E.Prim (E.Rest 0)) a = a
  horz a b = a E.:=: b

  mkNotes :: ScoreVoice -> E.Music E.Pitch
  mkNotes [] = E.rest 0
  mkNotes notes = foldr1 (E.:+:) (reverse (snd $ foldl mkNote (0, []) notes))

  mkNote :: (Rational, [E.Music E.Pitch]) 
    -> ((Rational, Rational), AtomicExpr) 
    -> (Rational, [E.Music E.Pitch])
  mkNote (e, n) ((l, r), Note p o) = 
    (r, if e == l then note:n else note:(E.rest ((l - e) / 4)):n)
      where
        note = E.note ((r - l) / 4) (p, o)

compToEuterpea = scoreToEuterpea . compToScore

main = E.play (compToEuterpea (comp [((0, 2, voices ["A"]), [Note E.D 3]), 
                                     ((4, 6, voices ["A"]), [Note E.E 3]),
                                     ((8, 12, voices ["A"]), [Note E.Fs 3]), 
                                     ((0, 2, voices ["S"]), [Note E.Fs 4]), 
                                     ((4, 6, voices ["S"]), [Note E.G 4]),
                                     ((8, 12, voices ["S"]), [Note E.A 4])]))

