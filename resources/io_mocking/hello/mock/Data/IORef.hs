{-# LANGUAGE Trustworthy #-}

module Data.IORef (
    IORef, newIORef, readIORef, writeIORef, modifyIORef, modifyIORef', atomicModifyIORef, atomicModifyIORef', atomicWriteIORef
  ) where

import Mock.System.IO.Internal
