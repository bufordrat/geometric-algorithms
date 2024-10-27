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

-- zeroComp :: (Num a, Eq a) => [a] -> [a] -> Ordering
-- zeroComp =
--   let zilch = (== 0)
--       zeroCount = length . takeWhile zilch
--   in compare `on` zeroCount

zeroCompPred :: (Num a, Eq a) => [a] -> [a] -> Bool
zeroCompPred lst1 lst2 =
  let zilch = (== 0)
      zeroCount = length . takeWhile zilch
  in zeroCount lst2 > zeroCount lst1

findWithIndex :: (a -> Bool) -> [a] -> Maybe (Int, a)
findWithIndex pred lst =
  let findWithIndex' _ _ [] = Nothing
      findWithIndex' idx pred (x : xs) =
        if pred x
        then Just (idx, x)
        else findWithIndex' (idx + 1) pred xs
  in findWithIndex' 0 pred lst

insertAt :: Int -> a -> [a] -> [a]
insertAt _ _ [] = []
insertAt 0 new lst = new : lst
insertAt n new (x : xs) = x : insertAt (n - 1) new xs

-- swap :: (Num a, Eq a) => [[a]] -> Maybe [[a]]
swap pred [] = Just []
swap pred mtrx@(row : rows) =
  let moreZeros r = pred row r
      rowToSwap = do
        (idx, r) <- findWithIndex moreZeros mtrx
        pure (row : insertAt idx r (delete row mtrx))
  in rowToSwap
  
