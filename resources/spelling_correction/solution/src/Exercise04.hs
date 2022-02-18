module Exercise04 where

import Data.List (group, minimum, minimumBy, nub, replicate, union, words)

{-H4.1.1-}
isMultiSet :: Eq a => [(a,Int)] -> Bool
isMultiSet xs = length (nub as) == length xs && and [x > 0 | (_,x) <- xs]
  where
    as = [x | (x,_) <- xs]

{-H4.1.2-}
toList :: [(a,Int)] -> [a]
toList [] = []
toList ((x,i):xs) = replicate i x ++ toList xs

{-H4.1.3-}
toSet :: Eq a => [(a, Int)] -> [a]
toSet mx = nub $ [ e | (e, _) <- mx ] 

{-H4.1.4-}
toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet xs =
  let filterEq x = [ x' | x' <- xs, x' == x ] in
  [ (x, length (filterEq x)) | x <- nub xs ]

{-H4.1.5-}
multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity _ [] = 0
multiplicity e ((e', m):xs) = if e == e' then m else multiplicity e xs

{-H4.1.6-}
dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct mx my = sum [ multiplicity e mx * multiplicity e my | e <- toSet mx `union` toSet my ]

{-H4.1.7-}
euclidean :: Eq a => [(a, Int)] -> Float
euclidean mx = sqrt $ fromIntegral (dotProduct mx mx)

{-H4.1.8-}
cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine mx my = fromIntegral (dotProduct mx my) / (euclidean mx * euclidean my)

{-H4.1.9-}
vocabSimilarity :: String -> String -> Float
vocabSimilarity ta tb = cosine (toMultiSet (words ta)) (toMultiSet (words tb))

{-H4.1.10-}
editDistanceAux :: Eq a => [a] -> [a] -> Int
editDistanceAux (x:xs) (y:ys) = let cost = if x == y then 0 else 1 in
  minimum [editDistance xs (y:ys) + 1, editDistance (x:xs) ys + 1, editDistance xs ys + cost]

editDistance :: Eq a => [a] -> [a] -> Int
editDistance [] ys = length ys 
editDistance xs [] = length xs 
editDistance (y1:y2:ys) (x1:x2:xs) | y1 == x2 && y2 == x1 && x1 /= y1 =
  minimum [editDistance ys xs + 1, editDistanceAux (y1:y2:ys) (x1:x2:xs)]
editDistance xs ys = editDistanceAux xs ys

frequentWords = 
    [
        "the", "at", "there", "some", "my",
        "of", "be", "use", "her", "than",
        "and", "this", "an", "would", "first",
        "a", "have", "each", "make", "water",
        "to", "from", "which", "like", "been",
        "in", "or", "she", "him", "call",
        "is", "one", "do", "into", "who",
        "you", "had", "how", "time", "oil",
        "that", "by", "their", "has", "its",
        "it", "word", "if", "look", "now",
        "he", "but", "will", "two", "find",
        "was", "not", "up", "more", "long",
        "for", "what", "other", "write", "down",
        "on", "all", "about", "go", "day",
        "are", "were", "out", "see", "did",
        "as", "we", "many", "number", "get",
        "with", "when", "then", "no", "come",
        "his", "your", "them", "way", "made",
        "they", "can", "these", "could", "may",
        "I", "said", "so", "people", "part",
        "Alice", "Bob"
    ] 

{-H4.1.11-}
spellCorrectWord :: [String] -> String -> [String]
spellCorrectWord dict s =
  let minDist = minimum [ editDistance s x | x <- dict ] in
  [ x | x <- dict, editDistance s x == minDist]

spellCorrect :: [String] -> [String] -> [[String]]
spellCorrect d xs = [spellCorrectWord d x | x<-xs]
