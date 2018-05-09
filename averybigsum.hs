main = interact $ show . sum . tail . map read . words 
