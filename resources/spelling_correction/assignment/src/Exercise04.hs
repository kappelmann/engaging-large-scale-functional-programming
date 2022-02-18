module Exercise04 where

{-H4.1.1-}
isMultiSet :: Eq a => [(a,Int)] -> Bool
isMultiSet = undefined

{-H4.1.2-}
toList :: [(a,Int)] -> [a]
toList = undefined

{-H4.1.3-}
toSet :: Eq a => [(a, Int)] -> [a]
toSet = undefined

{-H4.1.4-}
toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet = undefined

{-H4.1.5-}
multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity = undefined

{-H4.1.6-}
dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct = undefined

{-H4.1.7-}
euclidean :: Eq a => [(a, Int)] -> Float
euclidean = undefined

{-H4.1.8-}
cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine = undefined

{-H4.1.9-}
vocabSimilarity :: String -> String -> Float
vocabSimilarity = undefined

{-H4.1.10-}
editDistance :: Eq a => [a] -> [a] -> Int
editDistance = undefined

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
{-WETT-}
spellCorrect :: [String] -> [String] -> [[String]]
spellCorrect = undefined
{-TTEW-}
