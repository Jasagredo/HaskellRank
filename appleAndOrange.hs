
main = interact $ unlines . map show . filters . map read . words

filters :: [Int] -> [Int]
filters (s:t:a:b:m:n:fruits) = [apples, oranges]
	where apples = length $ filter (\x -> x >= s && x <= t) $ map (a+) $ take m $ fruits
	      oranges = length $ filter (\x -> x >= s && x <= t) $ map (b+) $ drop m $ fruits