module Combinatorics (choose, permute) where

choose :: (Integral a, Show a) => a -> a -> a
choose n r
  | n < 0 || r < 0 = error $ "choose: negative argument(s): n=" ++ show n ++ ", r=" ++ show r
  | r > n = 0
  | r == 0 = 1
  | 2 * r > n = n `choose` (n - r)
  | otherwise = h n r
  where
    h n' 1 = n'
    h n' r' = h (n' - 1) (r' - 1) * n' `div` r'

permute :: (Integral a, Show a) => a -> a -> a
permute n r
  | n < 0 || r < 0 = error $ "permute: negative argument(s): n=" ++ show n ++ ", r=" ++ show r
  | r > n = 0
  | r == 0 = 1
  | otherwise = product [n-r+1 .. n]
