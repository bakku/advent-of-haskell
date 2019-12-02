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

main = do
        contents <- readFile "01-input.txt"
        let instructions = map parseInt (split ',' (init contents))
        let initialState = replace (replace instructions 1 12) 2 2
        let result = executeIntCode initialState 0
        putStrLn (show (head result))
