{-# LANGUAGE Strict #-}
module Main where

import Control.Applicative (some)
import Data.Bits (Bits(shift))
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Tree as T
import Data.List (maximumBy, foldl', sort, sortOn)
import Data.Ord (comparing)
import Text.Parsec hiding (Empty)

data Rect = Rect { rectID     :: Int
                 , rectLeft   :: Int
                 , rectTop    :: Int
                 , rectWidth  :: Int
                 , rectHeight :: Int
                 }

instance Eq Rect where
  (==) = (==) `on` rectID

instance Ord Rect where
  compare = compare `on` rectID

instance Show Rect where
  show (Rect id l t w h) = "#" ++ show id ++ " " ++ show l ++ "," ++ show t ++ ":" ++ show (l+w) ++ "," ++ show (t+h)

inputP :: Parsec String () Rect
inputP =
  (\id (l,t) (w,h) -> Rect id l t w h)
  <$> (idP <* spaces <* char '@' <* spaces)
  <*> (posP <* char ':' <* spaces)
  <*> dimP
  where
    intP = read <$> some digit
    idP = char '#' *> intP
    posP = (,) <$> (intP <* char ',') <*> intP
    dimP = (,) <$> (intP <* char 'x') <*> intP

readInput :: [String] -> [Rect]
readInput ls = case traverse (parse inputP "") ls of
  Left e -> error (show e)
  Right rs -> rs

sheetSize :: [Rect] -> (Int, Int)
sheetSize rects =
  ( calcBound (\(Rect _ l _ w _) -> l + w)
  , calcBound (\(Rect _ _ t _ h) -> t + h))
  where
    calcBound f = f (maximumBy (comparing f) rects)

isOverlapCell :: [Rect] -> (Int, Int) -> Bool
isOverlapCell rects cell =
  (> 1) . length . take 2 . filter (cellInRect cell) $ rects
  where
    cellInRect (x, y) (Rect _ l t w h) =
      l <= x && (l+w) >= (x+1) && t <= y && (t+h) >= (y+1)

---------------- Brute force ----------------

bruteForceSolve :: [Rect] -> (Int, [Rect])
bruteForceSolve rects =
  let (w, h) = sheetSize rects
      cells = [(i, j) | i <- [0..w-1], j <- [0..h-1]]
      overlapArea = length . filter (isOverlapCell rects) $ cells
      noOverlapRects = filter (\r -> not $ any (\r1 -> r1 /= r && r1 `overlaps` r) rects) rects
  in (overlapArea, noOverlapRects)
  where
    (Rect _ l1 t1 w1 h1) `overlaps` (Rect _ l2 t2 w2 h2) =
      l1 < (l2+w2) && (l1+w1) > l2 && t1 < (t2+h2) && (t1+h1) > t2

---------------- Interval tree ----------------

newtype Interval a = Interval { unInterval :: (a,a) } deriving (Eq, Ord)

instance Show a => Show (Interval a) where
  show (Interval (a, b)) = "<" ++ show a ++ "," ++ show b ++ ">"

data IntervalTree a b = Node { itLeft :: IntervalTree a b
                             , itCenter :: a
                             , itIntervals :: [(Interval a, b)]
                             , itEndSortedIntervals:: [Interval a]
                             , itRight :: IntervalTree a b
                             }
                      | Empty a a deriving (Show, Eq)

rightOf, leftOf :: Ord a => Interval a -> a -> Bool
rightOf (Interval (start, _)) x = x < start
leftOf (Interval (_, end)) x = end <= x

insert :: (Ord a, Ord b, Bits a, Num a) => (Interval a, b) -> IntervalTree a b -> IntervalTree a b
insert o@(interval, _) tree = case tree of
  Empty start end -> go start end (start + half (end - start))
  Node l center is es r | interval `leftOf` center   -> Node (insert o l) center is es r
  Node l center is es r | interval `rightOf` center  -> Node l center is es (insert o r)
  Node l center is es r -> Node l center (sort (o:is)) (sortOn (negate . snd . unInterval) (interval:es)) r
  where
    go start end center
      | interval `leftOf` center   = Node (insert o (Empty start center)) center [] [] (Empty center end)
      | interval `rightOf` center  = Node (Empty start center) center [] [] (insert o (Empty center end))
      | otherwise                  = Node (Empty start center) center [o] [interval] (Empty center end)

    half = flip shift (-1)

overlappingIntervals :: Ord a =>
  (Interval a -> Interval a -> Bool) -> Interval a -> IntervalTree a b -> [(Interval a, b)]
overlappingIntervals f interval = go []
  where
    go acc t = case t of
      Empty _ _ -> acc
      Node l _      is _  _ | not (null is) && interval `leftOf` leftmostStart is -> go acc l
      Node _ _      _  es r | not (null es) && interval `rightOf` rightmostEnd es -> go acc r
      Node l center is _  _ | interval `leftOf` center  -> go (acc' is acc) l
      Node _ center is _  r | interval `rightOf` center -> go (acc' is acc) r
      Node l _      is _  r                             -> go (go (acc' is acc) l) r
      where
        acc' is acc = filter (\(i,_) -> i `f` interval) is ++ acc
        leftmostStart = fst . unInterval . fst . head
        rightmostEnd = snd . unInterval . head

includingIntervals :: Ord a => Interval a -> IntervalTree a b -> [(Interval a, b)]
includingIntervals =
  overlappingIntervals $ \(Interval (start1, end1)) (Interval (start2, end2)) ->
    start1 <= start2 && end2 <= end1

intersectingIntervals :: Ord a => Interval a -> IntervalTree a b -> [(Interval a, b)]
intersectingIntervals =
  overlappingIntervals $ \(Interval (start1, end1)) (Interval (start2, end2)) ->
    start2 < end1 && start1 < end2

fromList :: (Ord a, Ord b, Bits a, Num a) => a -> a -> [(Interval a, b)] -> IntervalTree a b
fromList start end = foldl' (flip insert) (Empty start end)

toInterval :: (Rect -> Int) -> (Rect -> Int) -> Rect -> Interval Int
toInterval pos dim rect = Interval (pos rect, pos rect + dim rect)

rectIntervalTrees :: [Rect] -> (IntervalTree Int Rect, IntervalTree Int Rect)
rectIntervalTrees rects =
  let (w, h) = sheetSize rects
  in ( fromList 0 w . map (\r -> (toInterval rectLeft rectWidth r, r)) $ rects
     , fromList 0 h . map (\r -> (toInterval rectTop rectHeight r, r)) $ rects
     )

toTree :: (Show a, Show b) => IntervalTree a b -> T.Tree String
toTree (Empty start end) = T.Node (show ("E", start, end)) []
toTree (Node l c is _ r) = T.Node (show ("N", c, is)) [toTree l, toTree r]

intervalTreeSolve :: [Rect] -> (Int, [Rect])
intervalTreeSolve rects =
  let (w, h) = sheetSize rects
      cells = [(i, j) | i <- [0..w-1], j <- [0..h-1]]
      (xTree, yTree) = rectIntervalTrees rects
      overlapArea = length . filter (\c -> isOverlapCell (cellRects xTree yTree c) c) $ cells
      noOverlapRects = filter ((== 1) . Set.size . overlappingRects xTree yTree) rects
  in (overlapArea, noOverlapRects)
  where
    cellRects xTree yTree (x,y) =
      nub . map snd
      $ includingIntervals (Interval (x, x+1)) xTree ++ includingIntervals (Interval (y, y+1)) yTree

    nub = Set.toList . Set.fromList

    rectIntervals tree pos dim rect =
      Set.fromList . map snd . intersectingIntervals (toInterval pos dim rect) $ tree

    overlappingRects xTree yTree rect =
      rectIntervals xTree rectLeft rectWidth rect `Set.intersection` rectIntervals yTree rectTop rectHeight rect

main :: IO ()
main = do
  rects <- readInput . lines <$> getContents
  let (overlapArea, noOverlapRects) = intervalTreeSolve rects
  putStrLn $ "Overlap Area = " ++ show overlapArea
  putStrLn $ "No overlap rects = " ++ show noOverlapRects