module Exercise04 where

rudolph :: String -> (Int, Int) 
rudolph s = if x == (0,0) then x else x
                where x = hlpEdf2 [] s (0,1)


hlpRdf :: String -> String -> (Int, Int) -> (Int,Int)
hlpRdf [] [] x = if x == (0,0) then (0,1) else x


hlpRdf ss st (n,m) = 
                            if returnedH then 
                                (if length ss > n then hlpRdf [] st (length ss,m+1) else hlpRdf [] st (n,m+1))                             
                            else if (validSS) then 
                                hlpRdf newSS xs (n,m)
                            else hlpRdf [] (attach ++ xs) (n,m)

            where   
                    newSS = ss ++ [x]
                    validSS = lROk && strOk
                    strOk = if length ss == 1 then (head ss) == 'R' else True
                    lROk = x `elem` "RL"
                    returnedH = length ([ r | r <- ss, r == 'R']) == length ([ l | l <- ss, l == 'L']) && (length ss) > 0 -- room for optimization
                    x = if st == [] then '-' else head st
                    xs = if st /= [] then tail st else []
                    attach = if ss == [] then [] else tail ss                




hlpEdf2 :: String -> String -> (Int, Int) -> (Int, Int)
hlpEdf2 [] [] x = x
hlpEdf2 ss@(x:xs) [] (n,m) = if retHm ss then retVal ss (n,m) else hlpEdf2 [] xs (n,m)
hlpEdf2 [] (y:ys) (n,m) = if isVaild [y] then hlpEdf2 [y] ys (n,m) else hlpEdf2 [] ys (n,m)
hlpEdf2 ss@(x:xs) st@(y:ys) (m,n)   | retHm ss = hlpEdf2 [] st (retVal ss (m,n))
                                    | isVaild (ss ++ [y]) = hlpEdf2 (ss ++ [y]) ys (m,n)
                                    | otherwise = hlpEdf2 [] (xs ++ st) (m,n)




isVaild :: String -> Bool
isVaild (x:xs) = (x `elem` "RL") && (if length (x:xs) == 1 then x == 'R' else True)

retHm :: String -> Bool
retHm s = length ([ r | r <- s, r == 'R']) == length ([ l | l <- s, l == 'L'])


retVal :: String -> (Int, Int) -> (Int, Int)
retVal s (n,m) = if (length s) > n then (length s, m+1) else (n,m)



teststr :: (Int,Int)
teststr = rudolph "LLR"



teststr2 :: (Int,Int)
teststr2 = rudolph "LRRRLLLLRRLRLL"


{-

if ss == [] then
                                hlpRdf [x] xs (n,m)
                            else 


-}