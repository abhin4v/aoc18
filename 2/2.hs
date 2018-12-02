module Main where

import Data.List (group, sort)

main = do
  ids <- lines <$> getContents
  print (length (repeatCounts 2 ids) * length (repeatCounts 3 ids))

  let diff = head [ diff | x <- ids, y <- ids
                         , let diff = zipWith (\ a b -> if a == b then a else '!') x y
                         , length (filter (== '!') diff) == 1 ]
  print $ filter (/= '!') diff
  where
    repeatCounts x = filter (x `elem`) . map (map length . group . sort)