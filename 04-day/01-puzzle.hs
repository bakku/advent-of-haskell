import Data.Maybe
import Data.List

subtractTuples :: (Num a) => [(a, a)] -> [a]
subtractTuples = map (\(i, j) -> j - i)

chunks :: [a] -> [(a, a)]
chunks (x:[]) = []
chunks (x:xs) = (x, head xs) : chunks xs

recTwoAdjacentDigits :: [Char] -> Int -> Bool
recTwoAdjacentDigits num pos
    | pos == length num = False
    | indexPairs == []  = recTwoAdjacentDigits num (pos + 1)
    | otherwise         = let distances = subtractTuples indexPairs
                          in isJust $ find (== 1) distances
    where curr = num !! pos
          indexPairs = chunks $ findIndices (== curr) num


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
