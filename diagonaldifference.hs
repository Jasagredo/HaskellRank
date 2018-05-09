group _ [] = []
group n x = (take n x) : (group n (drop n x))

parse = (map read . words)::(String -> [Int])
construct y = ([head y]) : group (head y) (tail y)
prinSum z = zipWith (!!) (tail z) [0..(head $ head z) -1]
secSum z = zipWith (!!) (tail z) (reverse [0..(head $ head z) -1])

main = interact $ show . (\z -> abs $ sum (prinSum z) - sum (secSum z)) . construct . parse
