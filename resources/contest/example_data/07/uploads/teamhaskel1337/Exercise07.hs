module Exercise07 where
import Data.List
import Data.Ord
type Graph = [[Integer]]

(#) :: Graph -> Graph -> Graph
(#) = (++)

emptyGraph :: Graph
emptyGraph = []

isTree :: ([Integer], [(Integer,Integer)]) -> Bool
isTree ([],[]) = True
isTree g | graphRemoved == g = False
         | otherwise = isTree graphRemoved
    where
        removeEnds (ns, es) = (nodes, edges)
            where 
                nodes = filter (\n -> (foldl (\s (e1,e2) -> if e1 == n || e2 == n then s+1 else s) 0 es) > 1) ns
                edges = filter (\(e1, e2) -> e1 `elem` nodes && e2 `elem` nodes) es
        graphRemoved = removeEnds g

constructMinGraph :: Integer -> ([Integer], [(Integer, Integer)]) -> [(Integer, Integer)] -> ([Integer], [(Integer, Integer)])
constructMinGraph 0 g _ = g
constructMinGraph n (ns, es) (ne:nes)
    | isTree constructedTree = constructMinGraph (n-1) constructedTree nes
    | otherwise = constructMinGraph n (ns,es) nes
    where
        constructedTree = (ns, ne : es)

parseList :: String -> Integer -> [Integer]
parseList [] x = [x]
parseList (' ':s) x = x : parseList s 0
parseList (i:s) x = parseList s ((read [i] :: Integer) + 10*x)

parseNLists :: Integer -> IO [[Integer]]
parseNLists n = sequence [do 
        line <- getLine
        return $ parseList line 0
     |x<-[0..n-1]]

printGraph :: [(Integer, Integer)] -> IO ()
printGraph [] = return ()
printGraph (e : es) = do
    putStr (show (snd e + 1))
    putStr " "
    putStrLn (show (fst e + 1))
    printGraph es

main :: IO ()
main = do
    nString <- getLine
    let n = read nString :: Integer

    lss <- parseNLists n
    let es = smallestEdge $ toGraph n 0 lss

    printGraph $ snd $ constructMinGraph (n-1) ([0..(n-1)], []) es

toGraph :: Integer -> Integer -> [[Integer]] -> [(Integer, Integer, Integer)]
toGraph maxN n _ | n == maxN = []
toGraph maxN n (xs : xss) =[ (w,to, n) | (to, w) <- zip [(n+1)..(maxN-1)] (drop (fromInteger (n+1)) xs)] ++ toGraph maxN (n+1) xss

smallestEdge :: [(Integer, Integer, Integer)] -> [(Integer, Integer)]
smallestEdge = map (\(w,f,t) -> (f,t)) . sortBy (comparing first)
    where first (a,_,_) = a

                 
                     
                