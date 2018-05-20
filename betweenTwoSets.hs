posible :: [Int]
posible = [1..100]

solve :: [Int] -> Int
solve (m:_:rest) =  length $ filter (\x -> (all (\y -> x `mod` y == 0) a) && (all (\y -> y `mod` x == 0) b)) posible
      where a = take m rest
      	    b = drop m rest

main = interact $ show . solve . map read . words