module Lib where

import Data.List (transpose, delete)
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

zeroComp :: (Num a, Eq a) => [a] -> [a] -> Bool
zeroComp lst1 lst2 =
  let zilch = (== 0)
      zeroCount = length . takeWhile zilch
  in zeroCount lst2 > zeroCount lst1

swapByIndex :: Eq a => Int -> [a] -> [a]
swapByIndex idx [] = []
swapByIndex idx lst | idx < 1 = lst
swapByIndex idx lst@(x : xs) =
  case safeBangBang idx lst of
    Just item ->
      let removed = delete item xs
      in item : delete item lst
    Nothing -> lst

-- for the actual Gaussian swap, use Data.List.find
