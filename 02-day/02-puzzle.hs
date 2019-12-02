import Data.List
import Data.Maybe

split :: Char -> [Char] -> [[Char]]
split _ []      = []
split s str     = let h = takeWhile (s /=) str
                      t = tail (dropWhile (s /=) str)
                  in if (h == str) then [h]
                     else [h] ++ (split s t)

parseInt :: [Char] -> Integer
parseInt x = read x :: Integer

replaceRec :: [Integer] -> Integer -> Integer -> Integer -> [Integer]
replaceRec [] _ _ _            = []
replaceRec (x:xs) pos new curr = if pos == curr then new : xs
                                 else x : (replaceRec xs pos new (curr + 1))

replace :: [Integer] -> Integer -> Integer -> [Integer]
replace list pos new = replaceRec list pos new 0

value :: [Integer] -> Int -> Integer
value code pos = code !! (fromInteger (code !! pos))

executeIntCode :: [Integer] -> Int -> [Integer]
executeIntCode code pos
    | (code !! pos) == 99 = code
    | (code !! pos) == 1  = executeIntCode (replace code (code !! (pos + 3)) (fst + snd)) (pos + 4)
    | (code !! pos) == 2  = executeIntCode (replace code (code !! (pos + 3)) (fst * snd)) (pos + 4)
    | otherwise           = error "invalid int code"
    where fst = value code (pos + 1)
          snd = value code (pos + 2)

possibleValues = concat (map (\x -> zip [0..99] (replicate 100 x)) [0..99])


expectedResult :: [Integer] -> (Integer, Integer) -> Integer -> Bool
expectedResult code (fst, snd) result = let initialState  = replace (replace code 1 fst) 2 snd
                                            resultingCode = executeIntCode initialState 0
                                        in result == (head resultingCode)

main = do
        contents <- readFile "01-input.txt"
        let instructions = map parseInt (split ',' (init contents))
            maybeResult  = find (\pair -> expectedResult instructions pair 19690720) possibleValues
            result       = fromMaybe (-1, -1) maybeResult
        putStrLn ((show (fst result)) ++ ", " ++ (show (snd result)))
