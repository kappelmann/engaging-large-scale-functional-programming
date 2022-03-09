module Exercise04 where
import Text.ParserCombinators.ReadP

rlParser :: ReadP Int
rlParser = do
    _ <- char 'R'
    len <- choice [rlParser, lrParser, triv]
    _ <- char 'L'
    return (2 + len)

triv :: ReadP Int
triv = do
    _ <- choice [string "RL", string "LR"]
    return 2

lrParser :: ReadP Int
lrParser = do
    _ <- char 'L'
    len <- choice [rlParser, lrParser, triv]
    _ <- char 'R'
    return (2 + len)

insParser :: ReadP Int
insParser = do
    choice [triv, lrParser, rlParser]

pathParser :: ReadP Int
pathParser = do
    _ <- skipMany $ satisfy $ const True
    _ <- char 'R'
    len <- insParser
    _ <- char 'L'
    return (2 + len)

rudolph :: String -> (Int, Int) 
rudolph s
    | null pathLengths = (0, 1)
    | otherwise = (maxPath, total)
    where
        pathLengths = map fst $ readP_to_S pathParser s
        maxPath = maximum pathLengths
        total = length $ filter (==maxPath) pathLengths
