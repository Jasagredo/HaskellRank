
main = interact $ unlines . map show . map rounding . map read . tail . words

rounding :: Int -> Int
rounding a
	 | (a > 37) && (a `mod` 5 > 2) = a + (5 - (a `mod` 5))
	 | otherwise = a