module Exercise04 where

selectAndReflectA :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
selectAndReflectA (i,j) xs = [(x,-y) | (x,y) <- xs, i <= x, x <= j]

selectAndReflectB :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
selectAndReflectB = undefined

selectAndReflectC :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
selectAndReflectC = undefined
