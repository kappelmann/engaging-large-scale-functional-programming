module Interface where

import Exercise04

{-H4.1.1-}
isMultiSet :: Eq a => [(a,Int)] -> Bool
isMultiSet = Exercise04.isMultiSet

{-H4.1.2-}
toList :: [(a,Int)] -> [a]
toList = Exercise04.toList

{-H4.1.3-}
toSet :: Eq a => [(a, Int)] -> [a]
toSet = Exercise04.toSet

{-H4.1.4-}
toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet = Exercise04.toMultiSet

{-H4.1.5-}
multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity = Exercise04.multiplicity

{-H4.1.6-}
dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct = Exercise04.dotProduct

{-H4.1.7-}
euclidean :: Eq a => [(a, Int)] -> Float
euclidean = Exercise04.euclidean

{-H4.1.8-}
cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine = Exercise04.cosine

{-H4.1.9-}
vocabSimilarity :: String -> String -> Float
vocabSimilarity = Exercise04.vocabSimilarity

{-H4.1.10-}
editDistance :: Eq a => [a] -> [a] -> Int
editDistance = Exercise04.editDistance

{-H4.1.11-}
spellCorrect :: [String] -> [String] -> [[String]]
spellCorrect = Exercise04.spellCorrect

