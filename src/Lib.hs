module Lib where

import Data.List (transpose)

type Matrix a = [[a]]

-- not necessary since the version in Data.List is better, just wrote
-- this for fun
transpose' :: Matrix a -> Matrix a
transpose' arr =
  [ arr >>= drop x . take (x + 1)
  | x <- [0..(maximum (map length arr) - 1)] ]
