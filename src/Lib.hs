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

zeroComp :: (Num a, Eq a) => [a] -> [a] -> Ordering
zeroComp =
  let zilch = (== 0)
      zeroCount = length . takeWhile zilch
  in compare `on` zeroCount

zeroCompPred :: (Num a, Eq a) => [a] -> [a] -> Bool
zeroCompPred lst1 lst2 =
  let zilch = (== 0)
      zeroCount = length . takeWhile zilch
  in zeroCount lst1 > zeroCount lst2

findWithIndex :: (a -> Bool) -> [a] -> Maybe (Int, a)
findWithIndex pred lst =
  let findWithIndex' _ _ [] = Nothing
      findWithIndex' idx pred (x : xs) =
        if pred x
        then Just (idx, x)
        else findWithIndex' (idx + 1) pred xs
  in findWithIndex' 0 pred lst

-- insertPred :: (Num a, Eq a) => (a -> Bool) -> a -> [a] -> [a]
-- insertPred _ _ [] = []
-- insertPred pred inserted (x : xs) =
--   if pred x
--   then x : inserted : xs
--   else x : insertPred pred inserted xs
  
-- swap :: (Num a, Eq a) => (a -> a -> Bool) -> [a] -> [a]
-- swap _ [] = []
-- swap bigger lst@(x : xs) =
--   let pred = not . bigger x
--       mayb = do
--         found <- find pred xs
--         pure $ found : delete found (insertPred pred x xs)
--   in case mayb of
--     Just output -> output
--     Nothing -> lst

-- Swap :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
-- swap _ [] = []
-- swap cmp (x : xs) =
--   let mayb = do
--         found <- find (\e -> cmp e x == GT) xs
--         pure (found : delete found (insertBy cmp x xs))
--   in case mayb of
--     Just output -> output
--     Nothing -> x : xs

