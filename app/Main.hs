module Main (main) where

import Solution (countCodewords, sortString)
import Numeric.Natural (Natural)
import Control.Monad (forever, forM_)
import Text.Read (readMaybe)

main :: IO ()
main = forever $ do
  putStr "String: "
  str <- getLine
  putStr "Codeword length: "
  lenStr <- getLine
  putStrLn ("\nGiven: " ++ sortString str ++ "\n")
  case (readMaybe lenStr :: Maybe Natural) of
    Nothing  -> putStrLn "Length must be a positive integer."
    Just len -> let (ans,strsLst) = countCodewords (fromIntegral len) str in do
      forM_ strsLst $ putStrLn . (\(a, b) -> a ++ ": " ++ b)
      putStrLn "------"
      putStrLn ("Total: " ++ show ans ++ "\n")