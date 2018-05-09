import Data.List

pos xs = map sum (map (map (xs !!)) [ [0..y]++[(y+2)..4] | y <- [x | x <- [(-1)..3]]])
main = interact $ (\z -> intercalate " " $ map show ((\y -> [minimum y, maximum y]) $ pos ((map read $ words z)::[Int])))
