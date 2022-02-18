module Exercise10 where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Map as Map (lookup)

import Turtle

-- data type for L-System
data Rule = Char :->: String                -- context-free and deterministic! 
  deriving (Eq,Show)

data LSystem = LSystem {
                  start :: String, 
                  rules :: [Rule]           -- constraint: unique left sides
                  }
  deriving Eq

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

-- finds the first occurrence of a fitting rule
findRule :: [Rule] -> Char -> Rule 
findRule = undefined

-- expands the L-System n times
expandLSystem :: LSystem -> Integer -> String
expandLSystem = undefined

-- updating LSystem via command
update :: LSystem -> IO LSystem
update ls = undefined

-- add the WETT ... TTEW tags if you want to participate in the wettbewerb!
