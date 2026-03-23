module Main (main) where

import Solution (countCodewords, sortString)
import Control.Monad (forever, forM_)
import Text.Read (readMaybe)

main :: IO ()
main = forever $ do
  putStr "String: "
  str <- getLine
  putStr "Codeword length: "
  lenStr <- getLine
  let lenError = putStrLn "Length must be a positive integer.\n" 
  case (readMaybe lenStr :: Maybe Int) of
    Nothing  -> lenError
    Just len -> if len <= 0 then lenError else do
      putStrLn ("\nNormalized string: " ++ sortString str ++ "\n")
      let (result, rows) = countCodewords len str
      forM_ rows $ (\(pattern, selection, permutations, products) ->
        putStrLn (show pattern ++ ": " ++ show selection ++ " x " ++ show permutations ++ " = " ++ show products))
      putStrLn "------"
      putStrLn ("Total: " ++ show result ++ "\n")