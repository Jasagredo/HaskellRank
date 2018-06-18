main = interact $ show . solve . map read . tail . words

findMax :: [Int] -> Int -> Int -> Int
findMax [] _ _ = 0
findMax l:ls m n
	     | m == l = n
	     | otherwise = findMax ls m (n+1)

solve :: [Int] -> Int
solve ls = findMax aparitions m 1
      	 where m = max aparitions
	       aparitions = map (length . flip ($) ls) $ map (\x -> filter (\y -> x == y)) [1..5]