module Exercise04 where

rudolph :: String -> (Int, Int)
rudolph [] = (0, 1)
rudolph s = rudiRec newS (0, 1)
    where   newS = dropWhile (=='L') s
            -- Assumes that c /= 'L'
            rudiRec :: String -> (Int, Int) -> (Int, Int)
            rudiRec [] currMax                      = currMax
            rudiRec s@(c:cs) oldMax@(oldMaxLen, oldNum)
                | feas && (currLen > oldMaxLen)     = rudiRec test (currLen, 1)
                | feas && (currLen == oldMaxLen)    = rudiRec test (currLen, oldNum + 1)
                | otherwise                         = rudiRec newCs oldMax
                where
                    newCs = dropWhile (=='L') cs
                    (feas, currLen) = validTill (c : cs)
                    test = if feas then drop currLen s else s


validTill :: String -> (Bool, Int)
validTill []        = (False, 0)
validTill ('L':rs)  = (False, 0)
validTill s         = validTillRec 0 0 s

-- s starts with r
-- return (True, k) if the first k chars are correct
-- (False, k) if we get to the end but not back at santas.
validTillRec :: Int -> Int -> String -> (Bool, Int)
-- If we are at the end of the string and at santas and have passed k chars -> return True, k
validTillRec k 0 [] = (True, k)
-- If we are at the end after k chars, but not at Santas -> False, k
validTillRec k _ [] = (False, k)
-- In this case there is still a string left
validTillRec k p (c:cs)
    -- If we went too far left, the last step was not valid -> we have to subtract one, but till then it was correct
    | p < 0             = (True, k - 1)
    -- If we reached the SW once we could stop, but maybe we can go even further?
    | p == 0 && k >= 1  = if res then furtherRes else (True, k) -- (True, k) -- 
    | otherwise         = furtherRes
        where
            furtherRes :: (Bool, Int)
            furtherRes
                -- If we have to go left, we consume this char and decrese the counter by 1
                | c == 'L'  = validTillRec (k + 1) (p - 1) cs
                -- Otherwise we consume it and increase the counter by 1
                | c == 'R'  = validTillRec (k + 1) (p + 1) cs
            -- res == True if there is a valid continuation of the current substring
            -- fk is the further k
            (res, fk) = furtherRes



------------- Not needed


-- return 0 = too far left, 1 = santas, 2 = valid
isValid :: String -> Int
isValid = isValidRec 0

-- return 0 = too far left, 1 = santas, 2 = valid
isValidRec :: Int -> String -> Int
isValidRec 0 []     = 2
isValidRec _ []     = 1     -- Didn't go back to santas
isValidRec k (c:cs)
    | k < 0     = 0         -- Did go too far left
    | c == 'L'  = isValidRec (k - 1) cs
    | otherwise = isValidRec (k + 1) cs