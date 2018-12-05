{-# LANGUAGE Strict #-}
module Main where

import Control.Applicative (some)
import Data.Bits (Bits(shift))
import qualified Data.Tree as T
import Data.List (maximumBy, foldl', nub)
import Data.Ord (comparing)
import Text.Parsec hiding (Empty)

import Data.Maybe (catMaybes)

data Rect = Rect { rectID     :: Int
                 , rectLeft   :: Int
                 , rectTop    :: Int
                 , rectWidth  :: Int
                 , rectHeight :: Int
                 } deriving (Eq)

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

isOverlapCell rects cell =
  (> 1) . length . take 2 . filter (cellInRect cell) $ rects
  where
    cellInRect (x, y) (Rect _ l t w h) =
      l <= x && (l+w) >= (x+1) && t <= y && (t+h) >= (y+1)

---------------- Brute force ----------------

bruteForceSolve rects =
  let (w, h) = sheetSize rects
      cells = [(i, j) | i <- [0..w-1], j <- [0..h-1]]
      overlapArea = length . filter (isOverlapCell rects) $ cells
  in overlapArea

---------------- Interval tree ----------------

newtype Interval a = Interval (a,a) deriving (Eq)

instance Show a => Show (Interval a) where
  show (Interval (a, b)) = "<" ++ show a ++ "," ++ show b ++ ">"

data IntervalTree a b = Node { itLeft :: IntervalTree a b
                             , itCenter :: a
                             , itIntervals :: [(Interval a, b)]
                             , itRight :: IntervalTree a b
                             }
                      | Empty a a deriving (Show, Eq)

insert :: (Ord a, Bits a, Num a) => (Interval a, b) -> IntervalTree a b -> IntervalTree a b
insert o@(interval, _) tree = case tree of
  Empty start end -> go start end (start + half (end - start))
  Node l center is r | interval `leftOf` center   -> Node (insert o l) center is r
  Node l center is r | interval `rightOf` center  -> Node l center is (insert o r)
  Node l center is r -> Node l center (o:is) r
  where
    go start end center
      | interval `leftOf` center   = Node (insert o (Empty start center)) center [] (Empty center end)
      | interval `rightOf` center  = Node (Empty start center) center [] (insert o (Empty center end))
      | otherwise                  = Node (Empty start center) center [o] (Empty center end)

    rightOf (Interval (start, _)) x = x < start
    leftOf (Interval (_, end)) x = end <= x
    half = flip shift (-1)

includingIntervals :: Ord a => Interval a -> IntervalTree a b -> [(Interval a, b)]
includingIntervals interval = go []
  where
    go acc t = case t of
      Empty _ _ -> acc
      Node l _ is r -> go (go (filter (\(i,_) -> i `includes` interval) is ++ acc) l) r

    includes (Interval (start1, end1)) (Interval (start2, end2))
      = start1 <= start2 && end2 <= end1

fromList :: (Ord a, Bits a, Num a) => a -> a -> [(Interval a, b)] -> IntervalTree a b
fromList start end = foldl' (flip insert) (Empty start end)

rectIntervalTrees :: [Rect] -> (IntervalTree Int Rect, IntervalTree Int Rect)
rectIntervalTrees rects =
  let (w, h) = sheetSize rects
  in ( fromList 0 w . map (\r -> (toInterval rectLeft rectWidth r, r)) $ rects
     , fromList 0 h . map (\r -> (toInterval rectTop rectHeight r, r)) $ rects
     )
  where
    toInterval pos dim rect = Interval (pos rect, pos rect + dim rect)

toTree :: (Show a, Show b) => IntervalTree a b -> T.Tree String
toTree (Empty start end) = T.Node (show ("E", start, end)) []
toTree (Node l c is r) = T.Node (show ("N", c, is)) [toTree l, toTree r]

intervalTreeSolve :: [Rect] -> Int
intervalTreeSolve rects =
  let (w, h) = sheetSize rects
      cells = [(i, j) | i <- [0..w-1], j <- [0..h-1]]
      (xTree, yTree) = rectIntervalTrees rects
      overlapArea = length . filter (\c -> isOverlapCell (cellRects xTree yTree c) c) $ cells
  in overlapArea
  where      
    cellRects xTree yTree (x,y) = 
      nub . map snd
      $ includingIntervals (Interval (x, x+1)) xTree ++ includingIntervals (Interval (y, y+1)) yTree

main :: IO ()
main = do
  rects <- readInput . lines <$> getContents
  let solution = intervalTreeSolve rects
  putStrLn $ "Overlap Area = " ++ show solution