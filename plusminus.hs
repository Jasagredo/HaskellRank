import Numeric
import Data.List

parse = (map read . words)::(String -> [Int])
tripleFilters x = map (flip ($) x) [filter (\y -> y > 0), filter (\y -> y < 0), filter (\y -> y == 0)]
fullSize t = (head. head $ t, (drop 1 . head . take 1 $ t):(drop 1 t))
relSizes m = map (\u ->  (fromIntegral $ length u)/(fromIntegral . fst $ m)) (snd m)
ffloat = map (\g -> showFFloat (Just 6) g "")

main = interact $ intercalate "\n" . ffloat . relSizes . fullSize . tripleFilters . parse
