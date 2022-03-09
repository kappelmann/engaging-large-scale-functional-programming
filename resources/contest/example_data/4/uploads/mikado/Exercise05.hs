module Exercise05 where

import Data.Sequence
import qualified Data.HashSet as HashSet

type Pos = (Int,Int)
data DDir = DUp | DDown | DLeft | DRight

showMaze :: Int -> Int -> [Pos] -> Pos -> Pos -> String
showMaze h w rocks start flag = unlines [ [showPos i j | j <- [0..w - 1]] | i <- [0..h - 1] ]
  where
    showPos i j
      | (i, j) `elem` rocks = 'X'
      | (i, j) == start = 'S'
      | (i, j) == flag = 'F'
      | otherwise = '.'

-- height, width, rocks, start, flag
incredibleGame :: Int -> Int -> [Pos] -> Pos -> Pos -> Int
incredibleGame h w rocks start flag = reverseSearch startSS startSet
  where 
    startSS = fromList [(flag,DUp,0),(flag,DDown,0),(flag,DLeft,0),(flag,DRight,0)]
    startSet = HashSet.empty
    fastRocks = HashSet.fromList rocks
    reverseSearch Empty _ = -1
    reverseSearch ((pos,dir,steps) :<| ss) set = case res of
        Nothing -> steps+1
        Just (ss',set') -> reverseSearch ss' set'
      where res = explore pos dir steps ss set

    -- explore :: Pos -> DDir -> Int -> Seq (Pos, DDir,Int) -> HashSet -> Maybe (Seq (Pos, DDir,Int),HashSet)
    explore pos dir steps ss set
        | n == start = Nothing
        | isStop n = Just (ss,set)
        | isStop a && isStop b = explore n dir steps ss set
        | isStop a = if checkSet a
          then explore n dir steps ss set 
          else  explore n dir steps (ss :|> (a,getLeft dir,steps+1)) (HashSet.insert a set)
        | isStop b = if checkSet b
          then explore n dir steps ss set 
          else  explore n dir steps (ss :|> (b,getRight dir,steps+1)) (HashSet.insert b set)
        | otherwise = explore n dir steps ss set
            where 
              mover = getMover dir
              checkSet pos' = HashSet.member pos' set
              (a,n,b) = mover pos

    isStop :: Pos -> Bool
    isStop (-1,_) = True 
    isStop (_,-1) = True 
    isStop (x_,y_) 
      | h == x_ = True 
      | w == y_ = True 
      | otherwise = HashSet.member (x_,y_) fastRocks

getLeft :: DDir -> DDir
getLeft DUp = DRight -- stop was on the left so go right
getLeft DDown = DLeft
getLeft DLeft = DUp
getLeft DRight = DDown

getRight :: DDir -> DDir
getRight DUp = DLeft
getRight DDown = DRight
getRight DLeft = DDown
getRight DRight = DUp

getMover :: DDir -> (Pos -> (Pos,Pos,Pos))
getMover DUp = \(x,y) -> let nextX = x-1 in ((nextX,y-1),(nextX,y),(nextX,y+1))
getMover DDown = \(x,y) -> let nextX = x+1 in ((nextX,y+1),(nextX,y),(nextX,y-1))
getMover DLeft = \(x,y) -> let nextY = y-1 in ((x+1,nextY),(x,nextY),(x-1,nextY))
getMover DRight = \(x,y) -> let nextY = y+1 in ((x-1,nextY),(x,nextY),(x+1,nextY))