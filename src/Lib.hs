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

insertByIndex :: Int -> a -> [a] -> [a]
insertByIndex _ _ [] = []
insertByIndex idx _ lst | idx < 1 = lst
insertByIndex 1 item (x : xs) = x : item : xs
insertByIndex idx item (x : xs) =
  x : insertByIndex (idx - 1) item xs

swapByIndex :: Eq a => Int -> [a] -> [a]
swapByIndex idx [] = []
swapByIndex idx lst | idx < 1 = lst
swapByIndex idx lst@(x : xs) =
  case safeBangBang idx lst of
    Just item ->
      let removed = delete item xs
      in item : delete item lst
    Nothing -> lst

