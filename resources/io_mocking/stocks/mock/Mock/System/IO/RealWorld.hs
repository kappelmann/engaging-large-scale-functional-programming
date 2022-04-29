{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Mock.System.IO.RealWorld (
    RealWorld,
    Direction (In, Out), HandleHook, ConsoleHook,
    newWorld, emptyWorld, setUser, runIO, evalIO, tryRunIO, tryEvalIO, runUser,
    runUserCompletely, dumpHandle, getOpenHandles, wait,
    registerWriteHook, hookConsole, readConsoleHook, showConsoleHook, hookHandle, readHandleHook, showHandleHook,
    tryIO, catchIO, setRandomSeed
  ) where

import Mock.System.IO.Internal

