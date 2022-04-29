{-# LANGUAGE Trustworthy #-}

module Control.Concurrent.MVar (
    MVar, newMVar, newEmptyMVar, isEmptyMVar, tryTakeMVar, takeMVar, tryPutMVar, tryReadMVar, 
    readMVar, putMVar, swapMVar, modifyMVar, modifyMVar_,
  ) where

import Mock.System.IO.Internal
