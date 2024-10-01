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

findWithIndex :: (a -> Bool) -> [a] -> Maybe (Int, a)
findWithIndex pred lst =
  let findWithIndex' _ _ [] = Nothing
      findWithIndex' idx pred (x : xs) =
        if pred x
        then Just (idx, x)
        else findWithIndex' (idx + 1) pred xs
  in findWithIndex' 0 pred lst

zeroComp :: (Num a, Ord a) => [a] -> [a] -> Ordering
zeroComp =
  let zilch = (== 0)
      zeroCount = length . takeWhile zilch
  in compare `on` zeroCount

compToPred :: (a -> a -> Ordering) -> a -> a -> Bool
compToPred comp x y =
  case comp x y of
    GT -> True
    EQ -> True
    LT -> False

swap :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
swap comp [] = []
swap comp lst@(x : xs) =
  let greaterThan = compToPred comp
  in case find (`greaterThan` x) xs of
       Just found -> insertBy comp x xs
       Nothing -> lst 
