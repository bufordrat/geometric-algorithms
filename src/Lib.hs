module Lib where

import Data.List (transpose, delete, insertBy, find)
import Data.Function (on)

type Matrix a = [[a]]

-- not necessary since the version in Data.List is better, just wrote
-- this for fun
transpose' :: Matrix a -> Matrix a
transpose' arr =
  [ arr >>= drop x . take (x + 1)
  | x <- [0..maximum (map length arr) - 1] ]

rows :: Matrix a -> [[a]]
rows = id

columns :: Matrix a -> [[a]]
columns = transpose

safeBangBang :: Int -> [a] -> Maybe a
safeBangBang _ [] = Nothing
safeBangBang 0 (x : _) = Just x
safeBangBang n (x : xs) = safeBangBang (n - 1) xs

get :: Int -> Int -> Matrix a -> Maybe a
get row col mx = do
  r <- safeBangBang row mx 
  c <- safeBangBang col r 
  pure c

get' :: (Int, Int) -> Matrix a -> Maybe a
get' = uncurry get

zeroComp :: (Num a, Ord a) => [a] -> [a] -> Ordering
zeroComp =
  let zilch = (== 0)
      zeroCount = length . takeWhile zilch
  in compare `on` zeroCount

swap :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
swap _ [] = []
swap cmp (x : xs) =
  let mayb = do
        found <- find (\e -> cmp e x == GT) xs
        pure (found : delete found (insertBy cmp x xs))
  in case mayb of
    Just output -> output
    Nothing -> x : xs
