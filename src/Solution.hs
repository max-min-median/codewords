module Solution (
  groupedCounts,
  repCountCombinations,
  showRepCombination,
  arrangements,
  selections,
  countCodewords,
  sortString,
) where

import qualified Data.Map as M
import Data.List (intercalate, sort)
import Data.Char (isAlphaNum, toUpper)
import Data.Tuple (swap)
import Combinatorics (choose, permute)

type RepCount = (Int, Int)


-- | Finds the grouped frequencies of alphanumeric characters in a given string.
--
-- Examples:
-- groupedCounts "Hello" --> [(2,1),(1,3)]  (1 pair, 3 uniques)
-- groupedCounts "Hello, World" --> [(3,1),(2,1),(1,5)]  (1 triple, 1 pair, 5 uniques)
groupedCounts :: String -> [RepCount]
groupedCounts s = M.toDescList $ M.fromListWith (+) $ [(rep,1) | (_,rep) <- M.toList (letterCounts s)]


-- letterCounts "Hello, World" --> fromList [('H',1),('W',1),('d',1),('e',1),('l',3),('o',2),('r',1)]
letterCounts :: String -> M.Map Char Int
letterCounts xs = M.fromListWith (+) $ [(toUpper x, 1) | x <- filter isAlphaNum xs]


sortString :: String -> String
sortString mp = concat . map (uncurry replicate) . reverse . sort . map swap . M.toList . letterCounts $ mp


-- | Finds all possible "poker-hand" combinations possible for a specified codeword length, given the grouped frequencies of the input string.
--
-- Examples:
-- repCountCombinations 3 [(2,1),(1,2)] --> [[(1,3)],[(2,1),(1,1)]]   patterns: 'ABC', 'AAB'
-- repCountCombinations 3 [(3,1),(2,1),(1,1)] --> [[(1,3)],[(2,1),(1,1)],[(3,1)]]   patterns: 'ABC', 'AAB', 'AAA'
-- repCountCombinations 5 [(3,1),(2,1),(1,1)] --> [[(2,2),(1,1)],[(3,1),(1,2)],[(3,1),(2,1)]]   patterns: ('AABBC', 'AAABC', 'AAABB')
repCountCombinations :: Int -> [RepCount] -> [[RepCount]]
repCountCombinations len = reverse . rCC len
  where
    rCC n _
      | n == 0 = [[]]
      | n < 0 = []
      -- n > 0 from here on
    rCC _ [] = []
    rCC n xss@((rep,freq): _) = h (min freq (n `div` rep))
      where
        use = useFromGroup xss
        h 0 = rCC n (use 0)
        h r = map ((rep, r):) (rCC (n - rep*r) (use r)) ++ h (r - 1)


-- Helper function to consume a certain amount of chunks from the first (most repetitious) rep-count in the list.
-- Unconsumed chunks are then treated as being 1 rep less (and combined with the next rep-count if appropriate).
useFromGroup :: [RepCount] -> Int -> [RepCount]
useFromGroup [] _ = error "useFromGroup: cannot use from []"
useFromGroup ((rep,freq): xs) n
  | n > freq = error $ "useFromGroup: cannot use " ++ show n ++ " from " ++ show (rep,freq)
  | rep == 1 = []
  | freq == n = xs
  | otherwise = case xs of []                -> [(rep-1,freq-n)]
                           (rep',freq'): xs' -> if rep-1 == rep' then (rep',freq'+freq-n): xs' else (rep-1,freq-n): (rep',freq'): xs'


-- | Pretty prints a rep-count as a string, where A, B, C etc. represent unique characters of the input string.
--
-- Examples:
-- showRepCombination [(2,1),(1,1)] --> "AAB"
-- showRepCombination [(4,1),(2,2)] --> "AAAABBCC"
showRepCombination :: [RepCount] -> String
showRepCombination xss = h 'A' xss
    where
      h _ [] = ""
      h ch ((rep,freq): xs)
        | freq == 0 = h ch xs
        | otherwise = replicate rep ch ++ h (succ ch) ((rep,freq-1): xs)


-- | Calculates the number of ways to select appropriate characters from the input string, in order to form a particular rep-count.
-- 
-- Examples:
-- selections [(3,1),(2,2),(1,1)] [(2,1),(1,1)] (9,"3C1 x 3C1")
-- Explanation: Given a string AAABBCCD, we are selecting a codeword of the format XXY. We have 3 ways of selecting X (A, B or C)
--              and a further 3 ways of selecting Y.
selections :: [RepCount] -> [RepCount] -> (Int, String)
selections strRepCounts codeRepCounts = let (res,strLst) = h 1 [] strRepCounts codeRepCounts in (res, intercalate " x " strLst)
  where
    h pdt s _ [] = (pdt, s)
    h _ _ [] _ = error "selections: unable to make combination"
    h pdt s sss@((sRep,sFreq): _) css@((cRep,cFreq): cs)
      | sRep > cRep = h pdt s (useFromGroup sss 0) css
      | sRep == cRep = h (pdt * sFreq `choose` cFreq) (s ++ [show sFreq ++ "C" ++ show cFreq]) (useFromGroup sss cFreq) cs
      | otherwise = error "selections: unable to make combination"


-- | Calculates the number of ways to permutate a string of a given rep-count
--
-- Examples:
-- arrangements [(1,5)] --> (120,"5!")   (arranging 'ABCDE')
-- arrangements [(3,1),(2,2)] --> (210,"7!/3!2!2!")   (arranging 'AAABBCC')
arrangements :: [RepCount] -> (Int, String)
arrangements xs = let (res,str) = h 1 "" n xs in (res, show n ++ "!" ++ (if null str then "" else "/" ++ str))
  where
    n = sum $ map (\(x, y) -> x * y) xs
    h pdt s _ [] = (pdt, s)
    h pdt s n' ((reps,freq): xs')
      | reps == 1 = (pdt * n' `permute` n', s)
      | otherwise = h (pdt * n' `choose` reps) (s ++ show reps ++ "!") (n'-reps) (if freq == 1 then xs' else (reps,freq-1): xs')


-- | Solves the codeword problem. Given a certain length and an input string, finds the number of possible codeword of that length
-- which can be form from the input string.
countCodewords :: Int -> String -> (Int, [(String, String)])
countCodewords n s = (sum . map fst $ combined, map snd combined)
  where
    strRepCounts = groupedCounts s
    rccs = repCountCombinations n strRepCounts
    -- feed each rcc into selections and arrangements
    selects = map (selections strRepCounts) rccs
    arrangemts = map arrangements rccs
    combined = zipWith3 (\(sels, selStr) (arrs, arrStr) rcc -> let pdt = sels * arrs in (pdt, (showRepCombination rcc, selStr ++ " x " ++ arrStr ++ " = " ++ show pdt))) selects arrangemts rccs