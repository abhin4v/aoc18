module Main where

import qualified Data.IntSet as Set

main = do
  changes <- map (read . dropPlus) . lines <$> getContents
  let sums = scanl (+) 0 . cycle $ changes
  putStrLn $ show $ sum changes
  putStrLn $ show $ firstDup sums

dropPlus ('+':cs) = cs
dropPlus cs = cs

firstDup :: [Int] -> Int
firstDup = go Set.empty
  where
    go seen (x:xs) = if x `Set.member` seen then x else go (Set.insert x seen) xs
