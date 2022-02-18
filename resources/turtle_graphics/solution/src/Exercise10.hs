module Exercise10 where

import Data.List (find, isPrefixOf, filter)
import Data.Maybe (fromMaybe)
import Data.Map as Map (lookup)
import System.IO (hReady, stdin)

import Turtle

-- data type for L-System
data Rule = Char :->: String                -- context-free and deterministic!
  deriving (Eq,Show)

data LSystem = LSystem {
                  start :: String, 
                  rules :: [Rule]           -- constraint: unique left sides
                  }
  deriving (Eq)

instance Show LSystem where
  show (LSystem s r) = unlines $ ["Start: " ++ show s, "Rules: "] ++ map show r


apply :: Char -> Turtle -> Turtle
apply 'F' t = move 10 t
apply 'G' t = move 10 t
apply '+' t = turn 30 t
apply '-' t = turn (-30) t
apply '*' t = turn 15 t
apply '~' t = turn (-15) t
apply _ t = sit t

-- Use apply to convert movements of turtle to GL lines.
-- Try changing the color to red! 
execute :: LSystem -> Integer -> [Ln]
execute ls n = let (pen, ang, pnt, ln, bps) = lines (black, 0, 0, [],[]) $ expandLSystem ls n in ln
  where
    lines t [] = t 
    lines t (x:xs) = lines (apply x t) xs

-- sample LSystems for (manual) testing
dragoncurve :: LSystem
dragoncurve = LSystem "FX" ['X' :->: "X+++YF+++", 'Y' :->: "---FX---Y"]

kochcurve :: LSystem
kochcurve = LSystem "F" ['F' :->: "F+++F---F---F+++F"]

sierpinski :: LSystem
sierpinski = LSystem "F++++G++++G" ['F' :->: "F++++G----F----G++++F", 'G' :->: "GG"]

ruleLhs :: Rule -> Char
ruleLhs (c :->: _) = c

ruleRhs :: Rule -> String
ruleRhs (_ :->: s) = s

findRule :: [Rule] -> Char -> Rule
findRule rs c = fromMaybe (c :->: [c]) (find (\x -> ruleLhs x == c) rs)

expandLSystem :: LSystem -> Integer -> String
expandLSystem ls n = expand (start ls) n
  where
    expand w 0 = w
    expand w n = expand (concatMap (ruleRhs . findRule (rules ls)) w) (n-1)
    
getReadyLine :: IO (Maybe String)
getReadyLine = do
  rdy <- hReady stdin
  if rdy then Just <$> getLine
         else return Nothing

update :: LSystem -> IO LSystem
update ls = do
  cmdM <- getReadyLine
  case cmdM of
    Just cmd -> execCmd cmd ls >>= update
    Nothing -> return ls
  where
    execCmd cmd ls = do
      let parsed = parseApplyCmd cmd ls
      case parsed of
        Left err -> putStrLn err >> return ls
        Right ls' -> return ls'


parseApplyCmd :: String -> LSystem -> Either String LSystem
parseApplyCmd s ls
  | "start " `isPrefixOf` s = Right $ LSystem (drop 6 s) $ rules ls
  | "rule " `isPrefixOf` s =
      case getRule (drop 5 s) of
        Nothing -> Left "Error parsing rule"
        Just rule -> Right $ LSystem (start ls) $ replaceRule rule ls 
  | s == "clear" = Right $ LSystem "" []
  | s == "print" = Left $ show ls 
  | otherwise = Left "Error parsing command"
  where
    replaceRule rule ls = rule : filter ((/= ruleLhs rule) . ruleLhs) (rules ls)

    getRule s = case words s of
                  [[s], "->", rhs] -> Just $ s :->: rhs
                  _ -> Nothing
