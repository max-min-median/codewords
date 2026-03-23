module Main (main) where

import Solution (countCodewords, sortString)
import Control.Monad (forM_)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStr "String: "
  str <- getLine
  if null str then return () else do
    putStr "Codeword length: "
    lenStr <- getLine
    let badLength = putStrLn "Length must be a positive integer.\n" 
    case readMaybe lenStr :: Maybe Int of
      Nothing  -> badLength
      Just len
        | len <= 0  -> badLength
        | otherwise -> do
            putStrLn $ "\nNormalized string: " ++ sortString str ++ "\n"
            let (result, rows) = countCodewords len str
            forM_ rows $ \(pattern, selection, permutations, rowTotal) ->
              putStrLn $ show pattern ++ ": " ++ show selection ++ " x " ++ show permutations ++ " = " ++ show rowTotal
            putStrLn "------"
            putStrLn $ "Total: " ++ show result ++ "\n"
    main