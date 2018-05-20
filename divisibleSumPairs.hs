solve :: (Int, [Int]) -> Int
solve (_, []) = 0
solve (k, x:xs) = (solve (k, xs) +) $ length $ filter (\y -> (x + y) `mod` k == 0) $ xs

main = interact $ show . solve . (\(x:xs) -> (x, xs)) . map read . tail . words