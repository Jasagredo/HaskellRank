import Data.List

parse = (map read . words)::(String -> [Int])
construct x = zip (take 3 x) (drop 3 x)
filters y = map (flip ($) y) [filter (\(x, y) -> x > y), filter (\(x, y) -> x < y)]
main = interact $ intercalate " " . map show . map length . filters . construct . parse
