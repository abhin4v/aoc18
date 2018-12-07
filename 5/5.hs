module Main where

import Data.Char
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Sequence as S
import Data.Sequence ((><), (|>))

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

isReactive :: Char -> Char -> Bool
isReactive x y = toLower x == toLower y && (isUpper x `xor` isUpper y)

simplify :: S.Seq Char -> S.Seq Char -> S.Seq Char
simplify acc s
  | S.null s        = acc
  | S.length s == 1 = acc >< s
  | S.length s >= 2  =
      let [x,y] = toList (S.take 2 s)
      in if isReactive x y 
         then simplify acc (S.drop 2 s)
         else simplify (acc |> x) (S.drop 1 s)

collapse :: S.Seq Char -> Int
collapse s =
  let simplified = simplify S.empty s
  in if S.length simplified == S.length s
     then S.length s
     else collapse simplified

betterCollapse :: S.Seq Char -> (Char, Int)
betterCollapse s =
  minimumBy (compare `on` snd)
  . map (\c -> (c, collapse $ S.filter (\c' -> toLower c' /= toLower c) s))
  $ ['a' .. 'z']

main = do
  input <- S.fromList . filter (/= '\n') <$> getContents
  putStrLn $ "Size = " ++ show (collapse input)
  putStrLn $ "Better Size = " ++ show (betterCollapse input)