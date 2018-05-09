step n m = (replicate (n-m) " ")++(replicate m "#")
stair n 1 = step n 1
stair n m = (stair n (m-1))++["\n"]++(step n m)

main = interact $ (\z -> concat $ (\x -> stair x x) ((read z)::Int))
