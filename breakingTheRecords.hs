solve :: [Int] -> (Int,Int) -> (Int, Int)
solve [] acc = (0,0)
solve (x:xs) (a, b)
      	     | x > a = let back = solve xs (x, b) in (1 + (fst back), snd back) 
	     | x < b = let back = solve xs (a, x) in (fst back, 1 + (snd back))
	     | otherwise = solve xs (a, b)

main = interact $ sol . map read . tail . words
     where sol = (\t -> (\(x,y) -> (show x) ++ " " ++ (show y)) $ solve (tail t) (head t, head t))
     	   