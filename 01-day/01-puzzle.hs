parseFloat :: String -> Float
parseFloat s = read s :: Float

calcFuel :: Float -> Integer
calcFuel x = floor (x / 3) - 2

main = do
    contents <- readFile "01-input.txt"
    let modules = map parseFloat (lines contents)
        requiredFuel = map calcFuel modules
        result = sum requiredFuel
    putStrLn (show result)
