module Combinatorics (choose, permute) where

import Numeric.Natural (Natural)

choose :: Natural -> Natural -> Natural
choose n r
  | r > n = 0
  | r == 0 = 1
  | 2 * r > n = n `choose` (n - r)
  | otherwise = go n r
  where
    go n' 1 = n'
    go n' r' = go (n' - 1) (r' - 1) * n' `div` r'

permute :: Natural -> Natural -> Natural
permute n r
  | r > n = 0
  | r == 0 = 1
  | otherwise = product [n-r+1 .. n]
