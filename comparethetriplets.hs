import Data.List

main = interact $ (\z -> intercalate " " $ map show $ map length $ (`map` [filter (\(x, y) -> x > y), filter (\(x, y) -> x < y)]) (\y -> ($) y ((\x -> zip (take 3 x) (drop 3 x)) $ ((map read $ words z)::[Int]))))
