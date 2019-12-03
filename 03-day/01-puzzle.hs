import Data.List
import Debug.Trace

parseInt :: [Char] -> Int
parseInt x = read x :: Int

split :: Char -> [Char] -> [[Char]]
split _ []      = []
split s str     = let h = takeWhile (s /=) str
                      t = tail $ dropWhile (s /=) str
                  in if (h == str) then [h]
                     else [h] ++ (split s t)

amountSteps :: (Num a) => [Char] -> [a]
amountSteps command = let amount = parseInt $ tail command
                      in replicate amount 1

goRight :: (Num a) => [(a, a)] -> a -> [(a, a)]
goRight positions step = let lastPos = last positions
                             newPos = (fst lastPos + step, snd lastPos)
                         in positions ++ [newPos]

goDown :: (Num a) => [(a, a)] -> a -> [(a, a)]
goDown positions step = let lastPos = last positions
                            newPos = (fst lastPos, snd lastPos + step)
                        in positions ++ [newPos]

goLeft :: (Num a) => [(a, a)] -> a -> [(a, a)]
goLeft positions step = let lastPos = last positions
                            newPos = (fst lastPos - step, snd lastPos)
                        in positions ++ [newPos]

goUp :: (Num a) => [(a, a)] -> a -> [(a, a)]
goUp positions step = let lastPos = last positions
                          newPos = (fst lastPos, snd lastPos - step)
                      in positions ++ [newPos]

walk :: (Num a) => [Char] -> (a, a) -> [(a, a)]
walk command initPos
    | "R" `isPrefixOf` command = foldl goRight [initPos] steps
    | "D" `isPrefixOf` command = foldl goDown [initPos] steps
    | "L" `isPrefixOf` command = foldl goLeft [initPos] steps
    | "U" `isPrefixOf` command = foldl goUp [initPos] steps
    where steps = amountSteps command

singleCommand :: (Num a) => [(a, a)] -> [Char] -> [(a, a)]
singleCommand positions command = let lastPos      = last positions
                                      newPositions = tail $ walk command lastPos
                                  in positions ++ newPositions

generateDetailedSteps :: (Num a) => [(a, a)] -> [[Char]] -> [(a, a)]
generateDetailedSteps = foldl singleCommand

mhDistance :: (Num a) => (a, a) -> (a, a) -> a
mhDistance (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

main = do
        contents <- readFile "01-input.txt"
        let wirePaths                    = map (split ',') $ lines contents
            [stepsWireOne, stepsWireTwo] = map (generateDetailedSteps [(0,0)]) wirePaths
            intersections                = intersect stepsWireOne stepsWireTwo
            distances                    = map (mhDistance (0, 0)) intersections
            minDistance                  = minimum $ tail distances
        print minDistance
