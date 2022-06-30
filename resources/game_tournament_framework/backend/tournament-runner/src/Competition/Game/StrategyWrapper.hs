{-# LANGUAGE ScopedTypeVariables #-}

module Competition.Game.StrategyWrapper (wrapStrategyEvaluation) where

import Competition.Config (moveTimeoutMillis)
import Competition.Types
import Competition.Util (getField, size)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq (($!!))
import Control.Exception (SomeException, evaluate, try)
import Control.Monad
import Data.Maybe (fromMaybe)
import GHC.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.Clock
import System.Random (newStdGen, randoms)
import System.Timeout (timeout)

-- Move result without duration
type TimelessMoveResult = Either MoveError Pos

wrapStrategyEvaluation :: StatefulStrategy a -> IoStrategy
wrapStrategyEvaluation statefulStrategy = try (evaluate statefulStrategy) >>= wrapStrategyEvaluationInner 

wrapStrategyEvaluationInner :: Either SomeException (StatefulStrategy a) -> IoStrategy
wrapStrategyEvaluationInner (Left ex) = return $ \_ _ -> return $ Left $ "Exception\n" ++ show ex
wrapStrategyEvaluationInner (Right (initialState, strategy)) = do
    state <- newIORef initialState
    time <- newIORef $ fromNanoSecs 0
    return $ wrapMoveEvaluation strategy state time
  where
    wrapMoveEvaluation :: StatefulStrategyFunc a -> IORef a -> IORef TimeSpec -> Player -> Board -> IO MoveResult
    wrapMoveEvaluation strategy stateIoRef time player board = timeoutMove time $ findAndValidateMove strategy stateIoRef player board

    findAndValidateMove :: StatefulStrategyFunc a -> IORef a -> Player -> Board -> IO TimelessMoveResult
    findAndValidateMove strategy stateIoRef player board = validateMove player board <$> findMove strategy stateIoRef player board

    findMove :: StatefulStrategyFunc a -> IORef a -> Player -> Board -> IO Pos
    findMove strategy stateIoRef player board = do
      gen <- newStdGen
      state <- readIORef stateIoRef
      let (pos, state') = strategy state (randoms gen) player board
      writeIORef stateIoRef state'
      return $!! pos

    validateMove :: Player -> Board -> Pos -> TimelessMoveResult
    validateMove player board pos@(y, x)
      | y < 0 || x < 0 || y >= h || x >= w = Left $ "OutOfBoundsMove\n" ++ show pos
      | signum (getField pos board) == negate player = Left $ "InvalidMove\n" ++ show pos
      | otherwise = Right pos
      where
        (h, w) = size board

    timeoutMove :: IORef TimeSpec -> IO TimelessMoveResult -> IO MoveResult
    timeoutMove = timeIo (fromNanoSecs $ fromIntegral $ moveTimeoutMillis * 1000000)

timer :: IO TimeSpec
timer = getTime ProcessCPUTime

timeIo :: TimeSpec -> IORef TimeSpec -> IO TimelessMoveResult -> IO MoveResult
timeIo timeout time io = do
  result <- newEmptyMVar
  start <- timer
  extraTime <- readIORef time
  cancellation <- forkIO (triggerTimeout timeout (timeout + start + extraTime) result)
  computation <- forkFinally io (addDuration start time timeout result)
  res <- takeMVar result
  killThread cancellation
  killThread computation
  return res

triggerTimeout :: TimeSpec -> TimeSpec -> MVar MoveResult -> IO ()
triggerTimeout ~to@(TimeSpec s ns) endTime result = do
  threadDelay 1000
  now <- timer
  if now > endTime 
    then void $ tryPutMVar result $ Left $ "Timeout\n" ++ show s ++ "\n" ++ show ns
    else triggerTimeout to endTime result

addDuration :: TimeSpec -> IORef TimeSpec -> TimeSpec -> MVar MoveResult -> Either SomeException TimelessMoveResult -> IO ()
addDuration _     _ _  result (Left ex) = void $ tryPutMVar result $ Left $ "Exception\n" ++ show ex
addDuration _     _ _  result (Right (Left e)) = void $ tryPutMVar result $ Left e
addDuration start t to result (Right (Right p)) = do
  end <- timer
  let time = end - start
  atomicModifyIORef' t (\o -> (o + to - time, ()))
  void $ tryPutMVar result $ Right (fromIntegral $ toNanoSecs time `div` 1000000, p)
