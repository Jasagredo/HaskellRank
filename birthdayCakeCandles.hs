parse = (map read . words)::(String -> [Int])
mainFilter x = filter (\y -> y == maximum x) x

main = interact $ show . length . mainFilter . drop 1 . parse
