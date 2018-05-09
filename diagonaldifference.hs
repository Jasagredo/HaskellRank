agr _ [] = []
agr n x = (take n x) : (agr n (drop n x))

main = interact $ show . (\x -> (\z -> abs (sum ( zipWith (!!) (tail z) [0..head $ head z]) - sum ( zipWith (!!) (tail z) (reverse [0..(head $ head z) -1])))) ((\y -> ([head y]) : agr (head y) (tail y)) ((map read $ words x)::[Int])))
