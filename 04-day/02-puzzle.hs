import Data.Maybe
import Data.List
import Debug.Trace

recTwoAdjacentDigits :: [Char] -> Int -> Bool
recTwoAdjacentDigits num pos
    | pos == length num   = False
    | length indices == 1 = recTwoAdjacentDigits num (pos + 1)
    | length indices > 2  = recTwoAdjacentDigits num (pos + 1)
    | otherwise           = True
    where curr       = num !! pos
          indices    = findIndices (== curr) num

twoAdjacentDigits :: (Num a, Show a) => a -> Bool
twoAdjacentDigits num = recTwoAdjacentDigits (show num) 0

recDigitsIncrease :: [Char] -> Bool
recDigitsIncrease (x:[]) = True
recDigitsIncrease (x:xs) = let fstNum = read [x] :: Int
                               sndNum = read $ [head xs] :: Int
                           in if fstNum <= sndNum then recDigitsIncrease xs
                              else False

digitsIncrease :: (Num a, Show a) => a -> Bool
digitsIncrease = recDigitsIncrease . show

validPassword :: (Num a, Show a) => a -> Bool
validPassword x = digitsIncrease x && twoAdjacentDigits x

main = do
        let possibilities         = [264793..803935]
            filteredPossibilities = filter validPassword possibilities
        print $ length filteredPossibilities
