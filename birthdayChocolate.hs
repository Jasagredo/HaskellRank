checkAndPass :: [Int] -> (Int, Int) -> Int
checkAndPass bar (day, month) = if length bar < month then 0
	     	       	      	   	      	      else add + (checkAndPass (tail bar) (day, month))
				where add = if sum (take month bar) == day then 1 else 0
				      	    	
allison :: [Int] -> Int
allison (l:rest) = checkAndPass pieces (day, month)
	where pieces = take l rest
	      md = drop l rest
	      day = head md
	      month = head . drop 1 $ md

main = interact $ show . allison . map read . words