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

data Claim = Claim { claimID     :: Int
                   , claimLeft   :: Int
                   , claimTop    :: Int
                   , claimWidth  :: Int
                   , claimHeight :: Int
                   }

instance Eq Claim where
  (==) = (==) `on` claimID

instance Ord Claim where
  compare = compare `on` claimID

instance Show Claim where
  show (Claim id l t w h) =
    "<#" ++ show id ++ " "
    ++ "(" ++ show l ++ "," ++ show t ++ ")-"
    ++ "(" ++ show (l+w) ++ "," ++ show (t+h) ++ ")>"

claimParser :: Parsec String () Claim
claimParser =
  (\id (l,t) (w,h) -> Claim id l t w h)
  <$> (idP <* spaces <* char '@' <* spaces)
  <*> (posP <* char ':' <* spaces)
  <*> dimP
  where
    intP = read <$> some digit
    idP  = char '#' *> intP
    posP = (,) <$> (intP <* char ',') <*> intP
    dimP = (,) <$> (intP <* char 'x') <*> intP

readInput :: String -> [Claim]
readInput input = case traverse (parse claimParser "") $ lines input of
  Left e   -> error (show e)
  Right rs -> rs

sheetSize :: [Claim] -> (Int, Int)
sheetSize claims = (calcBound claimRight, calcBound claimBottom)
  where
    claimRight  (Claim _ l _ w _) = l + w
    claimBottom (Claim _ _ t _ h) = t + h
    calcBound f = f (maximumBy (comparing f) claims)

isOverlapCell :: [Claim] -> (Int, Int) -> Bool
isOverlapCell claims cell =
  (> 1) . length . filter (cellInClaim cell) $ claims
  where
    cellInClaim (x, y) (Claim _ l t w h) =
      l <= x && (l+w) >= (x+1) && t <= y && (t+h) >= (y+1)

---------------- Brute force ----------------

bruteForceSolve :: [Claim] -> (Int, [Claim])
bruteForceSolve claims =
  let (width, height) = sheetSize claims
      cells           = [(i, j) | i <- [0..width-1], j <- [0..height-1]]
      overlapArea     = length . filter (isOverlapCell claims) $ cells
      noOverlapClaims =
        filter (\c -> not $ any (\c' -> c' /= c && c' `overlaps` c) claims) claims
  in (overlapArea, noOverlapClaims)
  where
    (Claim _ l1 t1 w1 h1) `overlaps` (Claim _ l2 t2 w2 h2) =
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

toInterval :: (Claim -> Int) -> (Claim -> Int) -> Claim -> Interval Int
toInterval pos dim claim = Interval (pos claim, pos claim + dim claim)

claimIntervalTrees :: [Claim] -> (IntervalTree Int Claim, IntervalTree Int Claim)
claimIntervalTrees claims =
  let (w, h) = sheetSize claims
  in ( fromList 0 w . map (\c -> (toInterval claimLeft claimWidth c, c)) $ claims
     , fromList 0 h . map (\c -> (toInterval claimTop claimHeight c, c)) $ claims
     )

toTree :: (Show a, Show b) => IntervalTree a b -> T.Tree String
toTree (Empty start end) = T.Node (show ("E", start, end)) []
toTree (Node l c is _ r) = T.Node (show ("N", c, is)) [toTree l, toTree r]

intervalTreeSolve :: [Claim] -> (Int, [Claim])
intervalTreeSolve claims =
  let (w, h) = sheetSize claims
      cells = [(i, j) | i <- [0..w-1], j <- [0..h-1]]
      (xTree, yTree) = claimIntervalTrees claims
      overlapArea = length . filter (\c -> isOverlapCell (cellClaims xTree yTree c) c) $ cells
      noOverlapClaims = filter ((== 1) . Set.size . overlappingClaims xTree yTree) claims
  in (overlapArea, noOverlapClaims)
  where
    cellClaims xTree yTree (x,y) =
      nub . map snd
      $ includingIntervals (Interval (x, x+1)) xTree ++ includingIntervals (Interval (y, y+1)) yTree

    nub = Set.toList . Set.fromList

    claimIntervals tree pos dim claim =
      Set.fromList . map snd . intersectingIntervals (toInterval pos dim claim) $ tree

    overlappingClaims xTree yTree claim =
      claimIntervals xTree claimLeft claimWidth claim `Set.intersection` claimIntervals yTree claimTop claimHeight claim

main :: IO ()
main = do
  claims <- readInput <$> getContents
  let (overlapArea, noOverlapClaims) = intervalTreeSolve claims
  putStrLn $ "Overlap Area = " ++ show overlapArea
  putStrLn $ "No overlap claims = " ++ show noOverlapClaims