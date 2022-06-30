module Types where

type Player = Int
type Field = Int
type Row = [Field]
type Column = [Field]
type Board = [Row]
type Pos = (Int, Int)
type Size = (Int, Int)
type Strategy = [Double] -> Player -> Board -> Pos
type StatefulStrategyFunc a = a -> [Double] -> Player -> Board -> (Pos, a)
type StatefulStrategy a = (a, StatefulStrategyFunc a)
