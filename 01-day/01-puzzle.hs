parseInt :: String -> Integer
parseInt s = read s :: Integer

calcFuel :: (Real a, Integral b) => a -> b
calcFuel x = floor (realToFrac x / 3) - 2

main = do
    contents <- readFile "01-input.txt"
    let modules = map parseInt (lines contents)
        requiredFuel = map calcFuel modules
        result = sum requiredFuel
    putStrLn (show result)
