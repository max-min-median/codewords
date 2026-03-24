module Combinatorics (choose, permute) where

import Numeric.Natural (Natural)


choose :: Natural -> Natural -> Natural
choose n r
  | r > n = 0
  | r == 0 = 1
  | 2 * r > n = n `choose` (n - r)
  | otherwise = let diff = n - r
                in foldl' (\acc x -> acc * (x + diff) `div` x) 1 [1..r]
                -- acc * (x + diff) is always divisible by x at each step


permute :: Natural -> Natural -> Natural
permute n r
  | r > n = 0
  | r == 0 = 1
  | otherwise = product [n-r+1 .. n]
