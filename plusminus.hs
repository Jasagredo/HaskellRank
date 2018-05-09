import Numeric
import Data.List

main = interact $  (\z -> intercalate "\n" (map (\g -> showFFloat (Just 6) g "") ((\(r,s) -> map (\u ->  (fromIntegral $ length u)/(fromIntegral r))  s) ((\t -> ((head. head $ t), drop 1 ( head (take 1 t)):(drop 1 t))) (map (\x -> x ((map read $ words z)::[Int])) [filter (\y -> y > 0), filter (\y -> y < 0), filter (\y -> y == 0)])))))
