module Solution (
  groupedCounts,
  possiblePatterns,
  permutePattern,
  selections,
  countCodewords,
  sortString,
) where

import qualified Data.Map as M
import Data.List (intercalate, sortOn, zip4)
import Data.Char (isAlphaNum, toUpper)
import Data.Tuple (swap)
import Combinatorics (choose, permute)

type RepCount = (Int, Int)

data Choice = Choice Int Int
newtype Selection = Selection [Choice]
data Permutation = Permutation { totalSlots :: Int, repetitions :: [Int] }  -- numerator, denominators
newtype Pattern = Pattern [RepCount]


-- | Finds the grouped frequencies of alphanumeric characters in a given string.
--
-- Examples:
-- groupedCounts "Hello" --> [(2,1),(1,3)]  (1 pair, 3 uniques)
-- groupedCounts "Hello, World" --> [(3,1),(2,1),(1,5)]  (1 triple, 1 pair, 5 uniques)
groupedCounts :: String -> [RepCount]
groupedCounts =
  M.toDescList
    . M.fromListWith (+)
    . map (\(_, reps) -> (reps, 1))
    . M.toList
    . letterCounts


-- | Count alphanumeric characters, case-insensitively.
--
-- Example:
-- letterCounts "Hello, World" --> fromList [('go',1),('W',1),('d',1),('e',1),('l',3),('o',2),('r',1)]
letterCounts :: String -> M.Map Char Int
letterCounts =
  M.fromListWith (+)
    . map (\ch -> (toUpper ch, 1))
    . filter isAlphaNum


sortString :: String -> String
sortString =
  intercalate " "
    . map (uncurry replicate)
    . sortOn (\(reps, ch) -> (-reps, ch))
    . map swap
    . M.toList
    . letterCounts


-- | Finds all possible "poker-hand" combinations possible for a specified codeword length, given the grouped frequencies of the input string.
--
-- Examples:
-- possiblePatterns 3 [(2,1),(1,2)] --> [[(1,3)],[(2,1),(1,1)]]   patterns: 'ABC', 'AAB'
-- possiblePatterns 3 [(3,1),(2,1),(1,1)] --> [[(1,3)],[(2,1),(1,1)],[(3,1)]]   patterns: 'ABC', 'AAB', 'AAA'
-- possiblePatterns 5 [(3,1),(2,1),(1,1)] --> [[(2,2),(1,1)],[(3,1),(1,2)],[(3,1),(2,1)]]   patterns: ('AABBC', 'AAABC', 'AAABB')
possiblePatterns :: Int -> [RepCount] -> [Pattern]
possiblePatterns codewordLength = map Pattern . go codewordLength
  where
    go remainingLength _
      | remainingLength == 0 = [[]]
      | remainingLength < 0 = []
    -- remainingLength > 0 from here on
    go _ [] = []
    go remainingLength repCounts@((rep,count): _) = concatMap tryUsing [0 .. maxUsed]
      where
        consume = consumeFromFirstGroup repCounts
        maxUsed = min count (remainingLength `div` rep)
        tryUsing 0 = go remainingLength (consume 0)
        tryUsing r = map ((rep, r):) (go (remainingLength - rep*r) (consume r))


-- Helper function to consume a certain amount of chunks from the first (most repetitious) rep-count in the list.
-- Unconsumed chunks are then treated as being 1 rep less (and combined with the next rep-count if appropriate).
consumeFromFirstGroup :: [RepCount] -> Int -> [RepCount]
consumeFromFirstGroup [] _ = error "consumeFromFirstGroup: cannot use from []"
consumeFromFirstGroup ((rep,count): xs) n
  | n > count = error $ "consumeFromFirstGroup: cannot use " ++ show n ++ " from " ++ show (rep,count)
  | rep == 1 = []
  | count == n = xs
  | otherwise = case xs of []                -> [(rep-1,count-n)]
                           (rep',freq'): xs' -> if rep-1 == rep' then (rep',freq'+count-n): xs' else (rep-1,count-n): (rep',freq'): xs'


-- Examples:
-- show (Pattern [(2,1),(1,1)]) --> "AAB"
-- show (Pattern [(4,1),(2,2)]) --> "AAAABBCC"
instance Show Pattern where
  show (Pattern repCounts) = concat $ zipWith replicate expandedCounts symbols
    where
      expandedCounts = concatMap (\(rep, count) -> replicate count rep) repCounts
      symbols = cycle (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])

-- | Calculates the number of ways to select appropriate characters from the input string, in order to form a particular rep-count.
-- 
-- Examples:
-- selections [(3,1),(2,2),(1,1)] [(2,1),(1,1)] --> Selection [(3,1),(3,1)]  (3C1 x 3C1)
-- Explanation: Given a string AAABBCCD, we are selecting a codeword of the format XXY. We have 3 ways of selecting X (A, B or C)
--              and a further 3 ways of selecting Y.
selections :: [RepCount] -> Pattern -> Selection
selections strRepCounts (Pattern patRepCounts)= Selection (go strRepCounts patRepCounts)
  where
    err = error "selections: unable to make combination"
    go _ [] = []
    go [] _ = err
    go sReps@((sRep,sFreq): _) cReps@((cRep,cFreq): cReps')
      | sRep > cRep = go (consumeFromFirstGroup sReps 0) cReps
      | sRep == cRep = Choice sFreq cFreq: go (consumeFromFirstGroup sReps cFreq) cReps'
      | otherwise = err


-- | Calculates the number of ways to permute a string of a given rep-count
--
-- Examples:
-- permutePattern [(1,5)] --> Permutation (5,[])   (arranging 'ABCDE' = 5! ways)
-- permutePattern [(3,1),(2,2)] --> Permutation (7,[3,2,2])   (arranging 'AAABBCC' = 7!/3!2!2! ways)
permutePattern :: Pattern -> Permutation
permutePattern (Pattern codeRepCount) = Permutation slots repeats
  where
    slots = sum $ map (\(x, y) -> x * y) codeRepCount
    repeats = concatMap (uncurry replicate) . map swap . filter (\(rep, _) -> rep > 1) $ codeRepCount


calculateSelections :: Selection -> Integer
calculateSelections (Selection nCrList) =
    fromIntegral . product . map (\(Choice n r) -> fromIntegral n `choose` fromIntegral r) $ nCrList


calculatePermutations :: Permutation -> Integer
calculatePermutations perm = go (totalSlots perm) (repetitions perm)
    where
      go num [] = fromIntegral (fromIntegral num `permute` fromIntegral num)
      go num (denom: denoms) = fromIntegral (fromIntegral num `choose` fromIntegral denom) * go (num - denom) denoms

instance Show Selection where
  show (Selection nCrList) = intercalate " x " . map (\(Choice n r) -> show n ++ "C" ++ show r) $ nCrList

instance Show Permutation where
  show (Permutation slots repeats)
    | null repeats = formatFactorial slots
    | otherwise = formatFactorial slots ++ "/" ++ concatMap formatFactorial repeats
    where
      formatFactorial x = show x ++ "!"


-- | Solves the codeword problem. Given a certain length and an input string, finds the number of possible codewords of that length
-- which can be formed from the input string.
countCodewords :: Int -> String -> (Integer, [(Pattern, Selection, Permutation, Integer)])
countCodewords codewordLength str = (result, zip4 patterns allSelections permutations products)
  where
    strRepCounts = groupedCounts str
    patterns = possiblePatterns codewordLength strRepCounts
    allSelections = map (selections strRepCounts) patterns
    permutations = map permutePattern patterns
    products = zipWith (*) (map calculateSelections allSelections) (map calculatePermutations permutations)
    result = sum products