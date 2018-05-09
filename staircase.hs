step n m = (replicate (n-m) " ")++(replicate m "#")

stair n 1 = step n 1
stair n m = (stair n (m-1))++["\n"]++(step n m)

staircase n = stair n n

parse = read::(String -> Int)
main = interact $ concat . staircase . parse
