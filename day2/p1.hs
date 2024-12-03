import qualified DayCommons as DC (parseInputFile)

data Tendency = Decreasing | Increasing

checkIsSafe :: [Integer] -> Maybe Integer -> Maybe Tendency -> Bool
checkIsSafe (x:xs) Nothing _ = checkIsSafe xs (Just x) Nothing
checkIsSafe (x:xs) (Just pv) (Just Decreasing) | x < pv && (pv-x) <= 3 && (pv-x) >= 1 = checkIsSafe xs (Just x) (Just Decreasing)
                                               | otherwise = False

checkIsSafe (x:xs) (Just pv) (Just Increasing) | x > pv && (x-pv) <= 3 && (x-pv) >= 1 = checkIsSafe xs (Just x) (Just Increasing)
                                               | otherwise = False

checkIsSafe (x:xs) (Just pv) Nothing | abs (x-pv) < 1 || abs (x-pv) > 3 = False
                                     | pv < x = checkIsSafe xs (Just x) (Just Increasing)
                                     | otherwise = checkIsSafe xs (Just x) (Just Decreasing)
checkIsSafe [] _ _ = True

main :: IO ()
main = do
    lines <- DC.parseInputFile "input.txt"
    print $ length $ filter (\l -> checkIsSafe l Nothing Nothing) lines