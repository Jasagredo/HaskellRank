import Data.List.Split

main = interact $ solve

toStr :: Int -> String
toStr a
      | a < 10 = '0':(show a)
      | otherwise = show a

solve :: String -> String
solve s = drop 1 $ concatMap (':':) . map toStr $ c
      where a = splitOn ":" s
      	    b = (\(x:xs) -> (drop 2 x ,(take 2 x):xs)) $ reverse a
	    c = case (fst b) of
	      	     	     "AM" -> (\(x:xs) -> (x `mod` 12):xs) $ reverse $ map read $ snd b
			     _ -> (\(x:xs) -> ((x `mod` 12) + 12):xs) $ reverse $ map read $ snd b