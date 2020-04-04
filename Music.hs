{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Music 
  (Coord(Coord), 
   v,
   In, 
   AtomicExpr(Note, Interval),
   Expr(In, EComp, Expr), 
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
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Control.Monad.Writer
import Control.Exception
import qualified Euterpea as E 

data Coord = 
  Coord { start :: Rational, end :: Rational, voices :: Set.Set String } 
  deriving (Eq, Show, Ord)
v :: [String] -> Set.Set String
v = Set.fromList

type In = (Coord, [Expr])
data AtomicExpr e = Note E.PitchClass Int 
                  | Interval Int
                  | Loop e
  deriving (Eq, Show, Ord)

data Expr = In In | EComp Comp | Expr (AtomicExpr TopExpr) deriving (Eq, Show)
newtype TopExpr = TopExpr [In] deriving (Eq, Show)

data Comp = Comp [Coord] (Map.Map Coord [AtomicExpr Comp]) deriving (Eq, Show, Ord)
emptyComp :: Comp
emptyComp = Comp [] Map.empty

type ScoreVoice = [((Rational, Rational), AtomicExpr Comp)]
type Score = Map.Map String ScoreVoice

data MusicException = MusicException deriving (Show)
instance Exception MusicException

comp :: [(Coord, [AtomicExpr Comp])] -> Comp
comp li = Comp (foldr go [] li) $ Map.fromListWith (++) $ reverse li
  where
    go :: (Coord, [AtomicExpr Comp]) -> [Coord] -> [Coord]
    go (coord, exprs) acc = (map (\_ -> coord) exprs) ++ acc

mergeCoords :: Coord -> Coord -> Coord
mergeCoords (Coord ps pe pvs) (Coord cs ce cvs) = 
  if | end > pe -> throw MusicException
     | not (cvs `Set.isSubsetOf` pvs) -> throw MusicException
     | otherwise -> Coord (cs + ps) end $ if (Set.null cvs) then pvs else cvs
  where end = ce + ps

offsetComp :: Coord -> Comp -> Comp
offsetComp pc (Comp o inner) = 
  Comp (map (mergeCoords pc) o) $ Map.mapKeys (mergeCoords pc) inner

translateCoord :: Rational -> Coord -> Coord
translateCoord offset (Coord s e v) = (Coord (s + offset) (e + offset) v)

translateComp :: Rational -> Comp -> Comp
translateComp offset (Comp o inner) = Comp (map trans o) (Map.mapKeys trans inner) 
  where trans = (translateCoord offset)

instance Semigroup Comp where 
  (<>) (Comp lOrder lMap) (Comp rOrder rMap) = 
    Comp (lOrder ++ rOrder) $ Map.unionWith (++) lMap rMap

instance Monoid Comp where mempty = emptyComp

compEnd :: Comp -> Rational
compEnd (Comp o inner) = foldr max 0 $ map end (Map.keys inner)

(<+>) l r = l <> translateComp (compEnd l) r

convertAExpr :: AtomicExpr TopExpr -> AtomicExpr Comp
convertAExpr (Loop e) = Loop (topExprToComp e)
convertAExpr (Note p o) = Note p o
convertAExpr (Interval i) = Interval i

exprToComp :: Coord -> Expr -> Comp
exprToComp pc (In (c, exprs)) = 
  mconcat (map (exprToComp (mergeCoords pc c)) exprs)
exprToComp pc (EComp comp') = offsetComp pc comp'
exprToComp c (Expr e) = Comp [c] $ Map.fromList [(c, [convertAExpr e])]

topExprToComp :: TopExpr -> Comp
topExprToComp (TopExpr ins) = 
  mconcat (map go ins) where 
    go (coord, exprs) = mconcat (map (exprToComp coord) exprs)

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
    go :: Coord -> [AtomicExpr Comp] -> Score -> Score
    go (Coord s e vs) exprs acc = 
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
    -> ((Rational, Rational), AtomicExpr Comp) 
    -> (Rational, [E.Music E.Pitch])
  mkNote (e, n) ((l, r), Note p o) = 
    (r, if e == l then note:n else note:(E.rest ((l - e) / 4)):n)
      where
        note = E.note ((r - l) / 4) (p, o)

compToEuterpea = scoreToEuterpea . compToScore

expandExpr :: Coord -> AtomicExpr Comp -> Comp
expandExpr c (Loop comp) = 
  mconcat $ take (floor (((end c) - (start c)) / len)) $ loop comp
  where len = compEnd comp
        loop comp = comp : loop (translateComp len comp)
expandExpr _ _ = emptyComp

expandComp :: Comp -> Comp
expandComp (Comp [] inner) = (Comp [] inner)
expandComp (Comp (o:os) inner) = 
  case (Map.lookup o inner) of Just (e:es) -> (go (expandedRest es) e) <> expandExpr o e
  where expandedRest es = expandComp $ (Comp os $ Map.insert o es inner)
        go (Comp restO restInner) e = (Comp (o:restO) $ Map.adjust ((:) e) o restInner)

test = (comp [(Coord 0 1 $ v ["A"], [Note E.D 3]), 
              (Coord 2 3 $ v ["A"], [Note E.E 3]),
              (Coord 4 6 $ v ["A"], [Note E.Fs 3]), 
              (Coord 0 1 $ v ["S"], [Note E.Fs 4]), 
              (Coord 2 3 $ v ["S"], [Note E.G 4]),
              (Coord 4 6 $ v ["S"], [Note E.A 4])])

bass = (comp [(Coord 0 (1/2) $ v ["B"], [Note E.D 2]),
              (Coord (1/2) 1 $ v ["B"], [Note E.A 2])])

main = E.play (compToEuterpea $ expandComp 
  ((test <+> test) <> (comp [(Coord 0 12 $ v ["B"], [Loop bass])])))

