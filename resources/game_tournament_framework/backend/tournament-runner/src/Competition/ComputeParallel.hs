{-# LANGUAGE BlockArguments #-}
module Competition.ComputeParallel where
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import System.Clock

-- Transform a list into an other type using parallel IO computations
-- additionally it can perform an IO action over partial results in regular intervals
runInParallel :: (Int, Int) -> ([b] -> IO ()) -> Int -> (a -> IO b) -> [a] -> IO [b]
runInParallel (s, ns) update numThreads compute values = do
  queue      <- newIORef values
  done       <- newIORef ([] :: [b])
  lastUpdate <- newIORef =<< getTime Monotonic
  running    <- mapM (const newEmptyMVar) threads
  mapM_ (\n -> forkFinally (whileJust_ (getShared queue) (runSingle done (sharedInterval interval lastUpdate) update compute)) (\_ -> putMVar (running !! n) ())) threads
  mapM_ (takeMVar . (running !!)) threads
  readIORef done
  where
    interval = TimeSpec (fromIntegral s) (fromIntegral ns)
    threads = [0 .. (numThreads - 1)]

-- compute a single value and run the update if necessary
runSingle :: IORef [b] -> IO Bool -> ([b] -> IO ()) -> (a -> IO b) -> a -> IO ()
runSingle sink shouldUpdate update compute v = do
  v' <- compute v
  putShared v' sink
  su <- shouldUpdate
  when su $ readIORef sink >>= update

-- put a value in shared queue
putShared :: a -> IORef [a] -> IO ()
putShared v = flip modifyIORef' (v:)

-- take a value from a shared queue
getShared :: IORef [a] -> IO (Maybe a)
getShared = flip atomicModifyIORef' go
  where
    go []     = ([], Nothing)
    go (x:xs) = (xs, Just x)

-- returns True at most once per interval
-- all calls that share a IORef must use the same interval
sharedInterval :: TimeSpec -> IORef TimeSpec -> IO Bool
sharedInterval interval ref = do
  now <- getTime Monotonic
  atomicModifyIORef' ref \old -> let next = old + interval in if next < now
    then (now, True)
    else (old, False)

-- https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html#whileJust_
whileJust_ :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
whileJust_ p f = p >>= maybe (return ()) (const (whileJust_ p f) <=< f)