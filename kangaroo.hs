main = interact $ kang . map read . words

kang :: [Int] -> String
kang (x1:v1:x2:v2:[]) = if v1 == v2 then "NO" else case res of
     		      	     	    	      	   True -> "YES"
				 		   _ -> "NO"
			where res = ((x1-x2) `mod` (v2-v1) == 0) && ((x1-x2) `div` (v2-v1) >= 0)