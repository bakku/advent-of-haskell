parseInt :: String -> Integer
parseInt s = read s :: Integer

calcFuel :: (Real a, Integral b) => a -> b
calcFuel x = floor (realToFrac x / 3) - 2

recCalcFuel :: (Real a, Integral b) => a -> b
recCalcFuel mass
    | fuel <= 0 = 0
    | otherwise = fuel + recCalcFuel fuel
    where fuel = calcFuel mass

main = do
    contents <- readFile "01-input.txt"
    let modules = map parseInt (lines contents)
        requiredFuel = map recCalcFuel modules
        result = sum requiredFuel
    putStrLn (show result)
