{-# LANGUAGE Unsafe #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Mock.System.IO.Internal (
    IO, MVar, IORef, Handle, IOMode (..), SeekMode (..), FilePath, HandlePosn (..), Direction (In, Out), SpecialFile (..),
    IOException (..), IOErrorType (..), ConsoleHook, HandleHook, BufferMode (..),
    RealWorld (RealWorld, handles, files, workDir, isPermitted),
    newWorld, emptyWorld, setUser, runIO, evalIO, tryRunIO, tryEvalIO, stdin, stdout, stderr, runUser, runUserCompletely,
    newMVar, newEmptyMVar, isEmptyMVar, tryTakeMVar, takeMVar, tryPutMVar, tryReadMVar, readMVar, putMVar, swapMVar, modifyMVar, modifyMVar_,
    newIORef, readIORef, writeIORef, modifyIORef, modifyIORef', atomicModifyIORef, atomicModifyIORef', atomicWriteIORef,
    withFile, openFile, hClose, readFile, writeFile, appendFile, doesFileExist,
    hFileSize, hSetFileSize, hIsEOF, isEOF, hGetBuffering, hSetBuffering, hFlush,
    hGetPosn, hSetPosn, hSeek, hTell, hReady, hAvailable, hWaitForInput,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable, hIsTerminalDevice, hShow,
    hGetChar, hGetLine, hLookAhead, hGetContents, 
    hPutChar, hPutStr, hPutStrLn, hPrint,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, readIO, readLn, interact,
    dumpHandle, getOpenHandles,wait,
    ioError, ioException, userError, tryIO, catchIO,
    registerWriteHook, hookConsole, readConsoleHook, showConsoleHook, hookHandle, readHandleHook, showHandleHook,
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation, 
    isPermissionError, isUserError, ioeGetErrorType, ioeGetLocation, ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    isReadDeadlockError, isAcceptDeadlockError,
    
    R.RandomGen(R.next, R.split, R.genRange), StdGen, R.mkStdGen, getStdRandom, getStdGen, setStdGen, 
    newStdGen, setRandomSeed
    
  ) where

import "base" Prelude hiding (FilePath, IO, getLine, getChar, readIO, readLn, putStr, putStrLn, putChar, print,
                              readFile, writeFile, appendFile, getContents, interact, userError, ioError, IOError)
import qualified "random" System.Random as R

import Control.Arrow
import Control.Applicative
import Control.Exception hiding (Deadlock, ioError, IOException, IOError)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Control.Monad.Pause
import Control.Monad.Except

import Data.Function
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord
import Data.List.Split
import Data.List
import Data.Typeable

import Foreign.C.Types

type FilePath = String

interpretPath :: FilePath -> FilePath -> FilePath
interpretPath p1 p2 = normalizePath $ if head p2 == '/' then p2 else p1 ++ "/" ++ p2

isValidPath :: FilePath -> Bool
isValidPath "" = False
isValidPath p = all (== '/') p || last p /= '/'

normalizePath :: FilePath -> FilePath
normalizePath p
    | abs p = fromPartsAbs (normAbs parts [])
    | otherwise = fromPartsRel (normRel parts [])
  where
    abs ('/':_) = True
    abs _ = False
    parts = filter (not . null) $ filter (/= ".") $ splitOn "/" $ p
    fromPartsAbs p = "/" ++ intercalate "/" p
    fromPartsRel p = if null p then "." else intercalate "/" p
    normAbs [] as = reverse as
    normAbs (".." : ps) [] = normAbs ps []
    normAbs (".." : ps) (a : as) = normAbs ps as
    normAbs (p : ps) as = normAbs ps (p : as)
    normRel [] as = reverse as
    normRel (".." : ps) [] = normRel ps [".."]
    normRel (".." : ps) (a : as)
        | a /= ".." = normRel ps as
    normRel (p : ps) as = normRel ps (p : as)

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode deriving (Show, Eq, Ord, Read, Enum, Typeable)
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd deriving (Show, Eq, Ord, Read, Enum, Typeable)

allowsReading :: IOMode -> Bool
allowsReading m = m == ReadMode || m == ReadWriteMode

allowsWriting :: IOMode -> Bool
allowsWriting m = m /= ReadMode


data SpecialFile = StdIn | StdOut | StdErr deriving (Eq, Ord, Typeable)

instance Show SpecialFile where
  show StdIn = "<stdin>"
  show StdOut = "<stdout>"
  show StdErr = "<stderr>"

data Direction = In | Out deriving (Eq, Ord, Show, Typeable)
data File = RegularFile FilePath | SpecialFile SpecialFile deriving (Eq, Ord, Typeable)

instance Show File where
  show (RegularFile p) = p
  show (SpecialFile t) = show t

data HandleType = SpecialHandle | RegularFileHandle deriving (Show, Eq, Ord)

data Handle = Handle {_hId :: Integer, _hName :: String, _hType :: HandleType,
                      _hInFile :: File, _hOutFile :: File} deriving (Typeable)

instance Eq Handle where
  (==) = (==) `on` _hId

instance Ord Handle where
  compare = comparing _hId

instance Show Handle where
  show = _hName

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int) deriving (Eq, Ord, Read, Show, Typeable)

data HandleData = HandleData {
  _hGetMode :: IOMode,
  _hIsOpen :: Bool,
  _hIsSeekable :: Bool,
  _hBufferMode :: BufferMode,
  _hInBufPos :: Integer,
  _hOutBufPos :: Integer
} deriving (Typeable)

_hIsFile :: Handle -> Bool
_hIsFile h = case _hInFile h of {RegularFile _ -> True; _ -> False}

type User = IO ()

data MVar a = MVar Integer deriving (Eq, Ord, Typeable)
data MValue = MEmpty | forall a. Typeable a => MValue a deriving (Typeable)

data IOErrorType = AlreadyExists | NoSuchThing | ResourceBusy | ResourceExhausted | EOF | IllegalOperation
  | PermissionDenied | UserError | UnsatisfiedConstraints | SystemError | ProtocolError | OtherError | InvalidArgument
  | InappropriateType | HardwareFault | UnsupportedOperation | TimeExpired | ResourceVanished | Interrupted
  | CrossingWorlds | ReadDeadlock | AcceptDeadlock
  deriving (Eq, Typeable)
  
instance Show IOErrorType where
  showsPrec _ e =
    showString $
    case e of
      AlreadyExists     -> "already exists"
      NoSuchThing       -> "does not exist"
      ResourceBusy      -> "resource busy"
      ResourceExhausted -> "resource exhausted"
      EOF               -> "end of file"
      IllegalOperation  -> "illegal operation"
      PermissionDenied  -> "permission denied"
      UserError         -> "user error"
      HardwareFault     -> "hardware fault"
      InappropriateType -> "inappropriate type"
      Interrupted       -> "interrupted"
      InvalidArgument   -> "invalid argument"
      OtherError        -> "failed"
      ProtocolError     -> "protocol error"
      ResourceVanished  -> "resource vanished"
      SystemError       -> "system error"
      TimeExpired       -> "timeout"
      UnsatisfiedConstraints -> "unsatisified constraints"
      UnsupportedOperation -> "unsupported operation"
      CrossingWorlds -> "not of this world"
      ReadDeadlock -> "read deadlock"
      AcceptDeadlock -> "socket accept deadlock"
      

data IOException = IOError {
   ioe_handle   :: Maybe Handle,   -- the handle used by the action flagging the error.
   ioe_type     :: IOErrorType,    -- what it was.
   ioe_location :: String,         -- location.
   ioe_description :: String,      -- error type specific information.
   ioe_errno    :: Maybe CInt,
   ioe_filename :: Maybe FilePath  -- filename the error is related to.
} deriving (Typeable)

instance Show IOException where
    showsPrec p (IOError hdl iot loc s _ fn) =
      (case fn of
         Nothing -> case hdl of
                        Nothing -> id
                        Just h  -> showsPrec p h . showString ": "
         Just name -> showString name . showString ": ") .
      (case loc of
         "" -> id
         _  -> showString loc . showString ": ") .
      showsPrec p iot .
      (case s of
         "" -> id
         _  -> showString " (" . showString s . showString ")")

instance Exception IOException

type IOError = IOException


{- IO definition -}

newtype IO a = IO { unwrapIO :: ExceptT IOException (PauseT (State RealWorld)) a } 
  deriving (Functor, Applicative, MonadError IOException, Typeable)

type StdGen = R.StdGen

data RealWorld = RealWorld {
  workDir :: FilePath,
  files :: Map File Text,
  isPermitted :: FilePath -> IOMode -> Bool,
  handles :: Map Handle HandleData,
  nextHandle :: Integer,
  user :: User,
  mvars :: Map Integer MValue,
  nextMVar :: Integer,
  writeHooks :: [Handle -> Text -> IO ()],
  theStdGen :: StdGen,
  _isUserThread :: Bool
} deriving (Typeable)

instance Monad IO where
  return = IO . return
  IO x >>= f = IO (x >>= (\x -> case f x of IO y -> y))

instance MonadFail IO where
  fail s = ioError (userError s)

{- IO errors -}

simpleIOError :: IOErrorType -> String -> String -> IO a
simpleIOError iot loc descr = throwError (IOError Nothing iot loc descr Nothing Nothing)

hIOError :: Handle -> IOErrorType -> String -> String -> IO a
hIOError h iot loc descr = throwError (IOError (Just h) iot loc descr Nothing Nothing)

fileIOError :: FilePath -> IOErrorType -> String -> String -> IO a
fileIOError path iot loc descr = throwError (IOError Nothing iot loc descr Nothing (Just path))

ioError :: IOError -> IO a
ioError = ioException

ioException :: IOException -> IO a
ioException = throwError

throwIO :: Exception e => e -> IO a
throwIO = throw

userError :: String -> IOError
userError s = IOError Nothing UserError "" s Nothing Nothing

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catchError

tryIO :: IO a -> IO (Either IOException a)
tryIO io = catchIO (fmap Right io) (return . Left)

ioeGetErrorType :: IOError -> IOErrorType
ioeGetErrorType = ioe_type

ioeGetErrorString :: IOError -> String
ioeGetErrorString ioe
   | isUserError ioe = ioe_description ioe
   | otherwise       = show (ioe_type ioe)

ioeGetLocation :: IOError -> String
ioeGetLocation ioe = ioe_location ioe

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle ioe = ioe_handle ioe

ioeGetFileName :: IOError -> Maybe FilePath
ioeGetFileName ioe = ioe_filename ioe

isAlreadyExistsError :: IOError -> Bool
isAlreadyExistsError = (== AlreadyExists) . ioeGetErrorType

isDoesNotExistError :: IOError -> Bool
isDoesNotExistError  = (== NoSuchThing) . ioeGetErrorType

isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError  = (== ResourceBusy) . ioeGetErrorType

isFullError         :: IOError -> Bool
isFullError          = (== ResourceExhausted) . ioeGetErrorType

isEOFError          :: IOError -> Bool
isEOFError           = (== EOF) . ioeGetErrorType

isIllegalOperation  :: IOError -> Bool
isIllegalOperation   = (== IllegalOperation) . ioeGetErrorType

isPermissionError   :: IOError -> Bool
isPermissionError    = (== PermissionDenied) . ioeGetErrorType

isUserError         :: IOError -> Bool
isUserError          = (== UserError) . ioeGetErrorType

isReadDeadlockError         :: IOError -> Bool
isReadDeadlockError          = (== ReadDeadlock) . ioeGetErrorType

isAcceptDeadlockError         :: IOError -> Bool
isAcceptDeadlockError          = (== AcceptDeadlock) . ioeGetErrorType

{- World manipulation -}

getWorld :: IO RealWorld
getWorld = IO (lift get)

getFromWorld :: (RealWorld -> a) -> IO a
getFromWorld f = IO (lift (gets f))

putWorld :: RealWorld -> IO ()
putWorld w = IO (lift (put w))

updateWorld :: (RealWorld -> RealWorld) -> IO ()
updateWorld f = IO (lift (modify f))

-- Give control back to the caller
wait :: IO ()
wait = IO pause

nop :: IO ()
nop = return ()

-- Runs an IO action until wait is called, storing the remaining action in the world using the given operation.
-- Returns True iff the action ran completely.
runIOUntilSuspend :: IO () -> (IO () -> IO ()) -> IO Bool
runIOUntilSuspend (IO c) wr = 
  IO (stepPauseT c) >>= either (\c -> wr (IO c) >> return False) (\_ -> wr nop >> return True)

tryEvalIO :: IO a -> RealWorld -> Either IOException a
tryEvalIO io w = fst (tryRunIO io w)

tryRunIO :: IO a -> RealWorld -> (Either IOException a, RealWorld)
tryRunIO (IO io) w = runState (runPauseT (runExceptT io)) w

runIO :: IO a -> RealWorld -> (a, RealWorld)
runIO io w = first (either (\e -> error ("Mock IO Exception: " ++ show e)) id) (tryRunIO io w)

evalIO :: IO a -> RealWorld -> a
evalIO io w = fst (runIO io w)


{- Random numbers -}

setStdGen :: StdGen -> IO ()
setStdGen sgen = updateWorld (\w -> w {theStdGen = sgen})

getStdGen :: IO StdGen
getStdGen = getFromWorld theStdGen

newStdGen :: IO StdGen
newStdGen =
  do (g, g') <- liftM R.split getStdGen
     setStdGen g
     return g'

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f =
  do (x, g) <- liftM f getStdGen
     setStdGen g
     return x

setRandomSeed :: Int -> IO ()
setRandomSeed seed = setStdGen (R.mkStdGen seed)

{- Mutable variables -}

newEmptyMVar :: Typeable a => IO (MVar a)
newEmptyMVar =
  do w <- getWorld
     let id = nextMVar w
     putWorld (w {nextMVar = id + 1, mvars = M.insert id MEmpty (mvars w)})
     return (MVar id)

newMVar :: Typeable a => a -> IO (MVar a)
newMVar y =
  do w <- getWorld
     let id = nextMVar w
     putWorld (w {nextMVar = id + 1, mvars = M.insert id (MValue y) (mvars w)})
     return (MVar id)

tryTakeMVar :: Typeable a => MVar a -> IO (Maybe a)
tryTakeMVar (MVar x) = 
  do y <- fmap (M.lookup x . mvars) getWorld
     case y of
       Nothing -> simpleIOError CrossingWorlds "tryTakeMVar" ("Invalid MVar: " ++ show x)
       Just MEmpty -> return Nothing
       Just (MValue y) -> case cast y of
                            Nothing -> simpleIOError CrossingWorlds "tryTakeMVar" ("Invalid MVar: " ++ show x)
                            Just y  -> return (Just y)

takeMVar :: Typeable a => MVar a -> IO a
takeMVar (MVar x) = tryTakeMVar (MVar x) >>= maybe (simpleIOError ReadDeadlock "takeMVar" ("Empty MVar: " ++ show x)) return

isEmptyMVar :: Typeable a => MVar a -> IO Bool
isEmptyMVar x = fmap (maybe True (const False)) (tryTakeMVar x)

tryReadMVar :: Typeable a => MVar a -> IO a
tryReadMVar = readMVar

readMVar :: Typeable a => MVar a -> IO a
readMVar = takeMVar

swapMVar :: Typeable a => MVar a -> a -> IO a
swapMVar x y = modifyMVar x (\y' -> return (y,y'))

modifyMVar :: Typeable a => MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (MVar x) f =
  do w <- getWorld
     case M.lookup x (mvars w) of
       Nothing -> simpleIOError ReadDeadlock "modifyMVar" ("Empty MVar: " ++ show x)
       Just MEmpty -> simpleIOError CrossingWorlds "modifyTakeMVar" ("Invalid MVar: " ++ show x)
       Just (MValue y) -> case cast y of
                             Nothing -> simpleIOError ReadDeadlock "modifyMVar" ("Empty MVar: " ++ show x)
                             Just y -> do (z,r) <- f y
                                          putWorld (w {mvars = M.insert x (MValue z) (mvars w)})
                                          return r

modifyMVar_ :: Typeable a => MVar a -> (a -> IO a) -> IO ()
modifyMVar_ x f = modifyMVar x (fmap (, ()) . f)

tryPutMVar :: Typeable a => MVar a -> a -> IO Bool
tryPutMVar (MVar x) y =
  do w <- getWorld
     putWorld (w {mvars = M.insert x (MValue y) (mvars w)})
     return True

putMVar :: Typeable a => MVar a -> a -> IO ()
putMVar x y = tryPutMVar x y >> return ()


{- IO References -}

newtype IORef a = IORef {ioRefToMVar :: MVar a} deriving (Eq, Ord, Typeable)

newIORef :: Typeable a => a -> IO (IORef a)
newIORef = fmap IORef . newMVar

readIORef :: Typeable a => IORef a -> IO a
readIORef (IORef v) = takeMVar v

writeIORef :: Typeable a => IORef a -> a -> IO ()
writeIORef (IORef v) x = putMVar v x

modifyIORef :: Typeable a => IORef a -> (a -> a) -> IO ()
modifyIORef (IORef v) f = modifyMVar_ v (return . f)

modifyIORef' :: Typeable a => IORef a -> (a -> a) -> IO ()
modifyIORef' (IORef v) f = modifyMVar_ v (\x -> let y = f x in y `seq` return y)

atomicModifyIORef :: Typeable a => IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef (IORef v) f = modifyMVar v (return . f)

atomicModifyIORef' :: Typeable a => IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' (IORef v) f = modifyMVar v (\x -> case f x of (y, z) -> y `seq` z `seq` return (y, z))

atomicWriteIORef :: Typeable a => IORef a -> a -> IO ()
atomicWriteIORef = writeIORef


{- Write Hooks -}

registerWriteHook :: (Handle -> Text -> IO ()) -> IO ()
registerWriteHook h = updateWorld (\w -> w {writeHooks = h : writeHooks w})

{- The User -}

setUser :: User -> IO ()
setUser = putUser

putUser :: User -> IO ()
putUser u = updateWorld (\w -> w {user = u})

defaultUser :: User
defaultUser = nop

reverseSpecialHandles :: IO ()
reverseSpecialHandles = updateWorld (\w -> w { handles = let hs = handles w in M.union (M.fromList (f hs)) hs})
  where f hs = zip specialHandles (map (fromJust . flip M.lookup hs) (drop 3 (cycle specialHandles)))

runUser :: IO Bool
runUser = 
  do u <- getFromWorld user
     reverseSpecialHandles
     updateWorld (\w -> w {_isUserThread = True})
     b <- runIOUntilSuspend u putUser
     updateWorld (\w -> w {_isUserThread = False})
     reverseSpecialHandles
     return b
     
runUserCompletely :: IO ()
runUserCompletely =
  do b <- runUser
     when (not b) runUserCompletely

isUserThread :: IO Bool
isUserThread = getFromWorld _isUserThread


mkSpecialHandle :: Integer -> SpecialFile -> Handle
mkSpecialHandle id t = Handle id (show t) SpecialHandle (SpecialFile t) (SpecialFile t)

specialHandles@[stdin, stdout, stderr, usrStdin, usrStdout, usrStderr] = 
    zipWith mkSpecialHandle [-1,-2..] [StdIn, StdOut, StdErr, StdIn, StdOut, StdErr] 

reverseSpecialHandle :: Integer -> Handle
reverseSpecialHandle i = cycle specialHandles `genericIndex` (2 - i)

newWorld :: FilePath -> [(FilePath, Text)] -> (FilePath -> IOMode -> Bool) -> RealWorld
newWorld workDir files permitted =
  RealWorld {
    workDir = workDir,
    files = M.fromList ([(SpecialFile t, "") | t <- [StdIn, StdOut, StdErr]] ++
                           [(RegularFile path, content) | (path, content) <- files]),
    isPermitted = permitted,
    nextHandle = 0,
    nextMVar = 0,
    handles = M.fromList [(stdin, HandleData ReadMode True False LineBuffering 0 0),
                           (stdout, HandleData AppendMode True False LineBuffering 0 0),
                           (stderr, HandleData AppendMode True False LineBuffering 0 0),
                           (usrStdin, HandleData WriteMode True False LineBuffering 0 0),
                           (usrStdout, HandleData ReadMode True False LineBuffering 0 0),
                           (usrStderr, HandleData ReadMode True False LineBuffering 0 0)],
    mvars = M.empty,
    user = defaultUser,
    writeHooks = [],
    theStdGen = R.mkStdGen 0,
    _isUserThread = False
  }

emptyWorld :: RealWorld
emptyWorld = newWorld "/" [] (\_ _ -> True)

getHData :: String -> Handle -> IO HandleData
getHData s h = do d <- fmap (M.lookup h . handles) getWorld
                  case d of
                    Nothing -> hIOError h CrossingWorlds s "Invalid handle"
                    Just d  -> return d

putHData :: Handle -> HandleData -> IO ()
putHData h d = do w <- getWorld
                  putWorld (w { handles = M.insert h d (handles w) })

hShow :: Handle -> IO String
hShow h =
  do d <- getHData "hShow" h
     let t = case _hGetMode d of
               ReadMode -> "readable"
               WriteMode -> "writable"
               AppendMode -> "writable (append)"
               ReadWriteMode -> "read-writable"
     let s = if _hIsOpen d then
               "{loc=" ++ show h ++ ",type=" ++ t ++ ",buffering=none}"
             else
               "{closed}"
     return s

hIsSeekable :: Handle -> IO Bool
hIsSeekable h = fmap _hIsSeekable (getHData "hIsSeekable" h)

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice h = case _hInFile h of SpecialFile t -> return (t == StdIn || t == StdOut || t == StdErr)
                                         _ -> return False

getFileContents :: String -> File -> IO Text
getFileContents s f = 
  do w <- getWorld
     case M.lookup f (files w) of
       Nothing -> case f of 
                    RegularFile p -> fileIOError p NoSuchThing s "No such file or directory"
                    _ -> simpleIOError NoSuchThing s "No such file or directory"
       Just t  -> return t

putFileContents :: File -> Text -> IO ()
putFileContents f t =
  do w <- getWorld
     putWorld (w {files = M.insert f t (files w)})

fileSize :: File -> IO Integer
fileSize f = fmap (fromIntegral . T.length) (getFileContents "fileSize" f)

                  
type HandlePosition = Integer
data HandlePosn = HandlePosn Handle HandlePosition deriving (Typeable)

instance Eq HandlePosn where
    (HandlePosn h1 p1) == (HandlePosn h2 p2) = p1==p2 && h1==h2

instance Show HandlePosn where
   showsPrec p (HandlePosn h pos) = 
        showsPrec p h . showString " at position " . shows pos

hEnsureOpen' :: String -> Handle -> HandleData -> IO ()
hEnsureOpen' s h d = 
  if _hIsOpen d then return () else hIOError h IllegalOperation s "handle is closed"

hEnsureOpen :: String -> Handle -> IO ()
hEnsureOpen s h = getHData s h >>= hEnsureOpen' s h

hIsOpen :: Handle -> IO Bool
hIsOpen h = fmap _hIsOpen (getHData "hIsOpen" h)

hIsClosed :: Handle -> IO Bool
hIsClosed h = fmap _hIsOpen (getHData "hIsClosed" h)

fileExists :: RealWorld -> File -> Bool
fileExists w f = (case f of {RegularFile p -> isValidPath p; _ -> True}) && M.member f (files w)

doesFileExist :: FilePath -> IO Bool
doesFileExist p = fileExists <$> getWorld <*> mkFile p

mkPath :: FilePath -> IO FilePath
mkPath path = fmap (\w -> interpretPath (workDir w) path) getWorld

mkFile :: FilePath -> IO File
mkFile = fmap RegularFile . mkPath

mkHandle :: String -> HandleType -> (Integer -> File) -> (Integer -> File) -> IOMode -> Bool -> Integer -> IO Handle
mkHandle name t inFile outFile mode seekable pos =
  do w <- getWorld
     let id = nextHandle w
     let h = Handle id name t (inFile id) (outFile id)
     let d = HandleData mode True seekable LineBuffering pos pos
     putWorld (w {nextHandle = id + 1, handles = M.insert h d (handles w)})
     return h

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p mode) then
       fileIOError path PermissionDenied "openFile" "Permission denied"
     else do
       let ex = fileExists w f
       when (not ex) $
         if mode == ReadMode then 
           fileIOError path NoSuchThing "openFile" "No such file or directory"
         else
           writeFile p ""
       pos <- if mode == AppendMode then fmap (fromIntegral . T.length) (getFileContents "openFile" f) else return 0
       mkHandle p RegularFileHandle (const f) (const f) mode True pos

hClose :: Handle -> IO ()
hClose h = 
  do d <- getHData "hClose" h
     putHData h (d {_hIsOpen = False})

getOpenHandles :: IO [Handle]
getOpenHandles = 
  do w <- getWorld
     return [h | (h, d) <- M.toList (handles w), _hIsOpen d]

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode f =
  do h <- openFile path mode
     r <- f h
     hClose h
     return r

hTell :: Handle -> IO Integer
hTell h =
  do d <- getHData "hTell" h
     hEnsureOpen' "hTell" h d
     if _hIsSeekable d then 
       return (fromIntegral (_hOutBufPos d)) 
     else
       hIOError h IllegalOperation "hTell" "handle is not seekable"

hFileSize :: Handle -> IO Integer
hFileSize h
  | _hIsFile h = 
      do d <- getHData "hFileSize" h
         hEnsureOpen' "hFileSize" h d
         t <- getFileContents "hFileSize" (_hInFile h)
         return (fromIntegral (T.length t))
  | otherwise = hIOError h InappropriateType "hFileSize" "not a regular file"

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering h = fmap _hBufferMode (getHData "hGetBuffering" h)

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering h m = getHData "hSetBuffering" h >>= (\d -> putHData h (d { _hBufferMode = m}))

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = HandlePosn h <$> hTell h

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn h p) = hSeek h AbsoluteSeek p

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h mode pos =
  do d <- getHData "hSeek" h
     hEnsureOpen' "hSeek" h d
     when (not (_hIsSeekable d)) (hIOError h IllegalOperation "hSeek" "handle is not seekable")
     t <- getFileContents "hSeek" (_hInFile h)
     let size = fromIntegral (T.length t)
     let pos' = case mode of
                  AbsoluteSeek -> fromIntegral pos
                  RelativeSeek -> _hOutBufPos d + fromIntegral pos
                  SeekFromEnd  -> size - fromIntegral pos
     when (pos' < 0) (hIOError h InvalidArgument "hSeek" "Invalid argument")
     putHData h (d {_hInBufPos = pos', _hOutBufPos = pos'})

hIsEOF :: Handle -> IO Bool
hIsEOF h =
  do d <- getHData "hIsEOF" h
     hEnsureOpen' "hIsEOF" h d
     t <- getFileContents "hIsEOF" (_hInFile h)
     return (_hInBufPos d >= fromIntegral (T.length t))

hReady :: Handle -> IO Bool
hReady h = 
  do hWaitForInput h 0
     fmap not (hIsEOF h)

hAvailable :: Handle -> IO Integer
hAvailable h =
  do d <- getHData "hAvailable" h
     hEnsureOpen' "hAvailable" h d
     hEnsureReadable "hAvailable" h d
     t <- getFileContents "hAvailable" (_hInFile h)
     return (max 0 (fromIntegral (T.length t) - _hInBufPos d))

isEOF :: IO Bool
isEOF = hIsEOF stdin

hIsReadable :: Handle -> IO Bool
hIsReadable h = (allowsReading . _hGetMode) <$> getHData "hIsReadable" h

hIsWritable :: Handle -> IO Bool
hIsWritable h = (allowsWriting . _hGetMode) <$> getHData "hIsWritable" h

dumpHandle :: Handle -> Direction -> IO Text
dumpHandle h d = 
  do hEnsureOpen "dumpHandle" h
     getFileContents "dumpHandle" (if d == In then _hInFile h else _hOutFile h)

_hSetFileSize :: String -> Handle -> HandleData -> Integer -> IO ()
_hSetFileSize s h d size =
  do hEnsureOpen' s h d
     when (not (allowsWriting (_hGetMode d))) $
        if h == stdout || h == stderr then
          hIOError h IllegalOperation s ("user cannot write to " ++ show h)
        else 
          hIOError h IllegalOperation s "handle is not open for writing"
     t <- getFileContents s (_hOutFile h)
     let diff = fromIntegral size - fromIntegral (T.length t)
     case compare diff 0 of
       EQ -> return ()
       GT -> putFileContents (_hOutFile h) (T.append t (T.replicate diff (T.singleton '\0')))
       LT -> putFileContents (_hOutFile h) (T.take (fromIntegral size) t)

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize h size = 
  do d <- getHData "hSetFileSize" h
     _hSetFileSize "hSetFileSize" h d size

hPrepareWrite :: String -> Handle -> HandleData -> IO ()
hPrepareWrite s h d = _hSetFileSize s h d (_hOutBufPos d)

hPutText :: Handle -> Text -> IO ()
hPutText h s =
  do d <- getHData "hPutText" h
     hPrepareWrite "hPutText" h d
     let l = T.length s
     t <- getFileContents "hPutText" (_hOutFile h)
     let t' = case T.splitAt (fromIntegral (_hOutBufPos d)) t of
                (t1, t2) -> T.append t1 (T.append s (T.drop l t2))
     putHData h (d {_hOutBufPos = _hOutBufPos d + fromIntegral l})
     putFileContents (_hOutFile h) t'
     hooks <- getFromWorld writeHooks
     mapM_ (\hook -> hook h s) hooks

hPutStr :: Handle -> String -> IO ()
hPutStr h s = hPutText h (T.pack s)

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutText h (T.pack (s ++ "\n"))

hPutChar :: Handle -> Char -> IO ()
hPutChar h c = hPutText h (T.singleton c)

hPrint :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)

print :: Show a => a -> IO ()
print = hPrint stdout

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

putChar :: Char -> IO ()
putChar = hPutChar stdout


hEnsureReadable :: String -> Handle -> HandleData -> IO ()
hEnsureReadable s h d
  | not (_hIsOpen d) = hIOError h IllegalOperation s "handle is closed"
  | allowsReading (_hGetMode d) = return ()
  | otherwise = if h == stdin then
                  hIOError h IllegalOperation s "handle is not open for reading"
                else
                  hIOError h IllegalOperation s "user cannot read from stdin"

_hGetText :: Handle -> Integer -> IO Text
_hGetText h s =
  do d <- getHData "hGetText" h
     hEnsureReadable "hGetText" h d
     t <- getFileContents "hGetText" (_hInFile h)
     if _hInBufPos d + fromIntegral s > fromIntegral (T.length t) then
       hIOError h EOF "hGetText" ""
     else do
       let t' = T.take (fromIntegral s) (T.drop (fromIntegral (_hInBufPos d)) t)
       putHData h (d {_hInBufPos = _hInBufPos d + fromIntegral s})
       return t'

_hLookAhead :: Handle -> IO Char
_hLookAhead h =
  do d <- getHData "hLookAhead" h
     hEnsureReadable "hLookAhead" h d
     t <- getFileContents "hLookAhead" (_hInFile h)
     if _hInBufPos d >= fromIntegral (T.length t) then
       hIOError h EOF "hLookAhead" ""
     else
       return (T.index t (fromIntegral (_hInBufPos d)))

hGetChar :: Handle -> IO Char
hGetChar h = fmap T.head (hGetText h 1)

hGetContentText :: Handle -> IO Text
hGetContentText h = do {t <- aux; hClose h; return t}
  where aux =
          do res <- wrapBlockingOp' "hGetContexts" op h
             case res of
               Just t | not (T.null t) -> return t 
               _ -> return T.empty
        op h =
          do d <- getHData "hGetContexts" h
             hEnsureReadable "hGetContents" h d
             t <- getFileContents "hGetContents" (_hInFile h)
             let t' = T.drop (fromIntegral (_hInBufPos d)) t
             putHData h (d {_hInBufPos = _hInBufPos d + fromIntegral (T.length t')})
             return t'

_hGetLineText :: Handle -> IO Text
_hGetLineText h =
  do d <- getHData "hGetLine" h
     hEnsureReadable "hGetLine" h d
     t <- getFileContents "hGetLine" (_hInFile h)
     if _hInBufPos d >= fromIntegral (T.length t) then
       hIOError h EOF "hGetLine" ""
     else do
       let (t1, t2) = T.span (/= '\n') (T.drop (fromIntegral (_hInBufPos d)) t)
       let s = fromIntegral (T.length t1) + (if T.isPrefixOf "\n" t2 then 1 else 0)
       putHData h (d {_hInBufPos = _hInBufPos d + s})
       return t1

hGetLine :: Handle -> IO String
hGetLine h = fmap T.unpack (hGetLineText h)

readFileText :: FilePath -> IO Text
readFileText path =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p ReadMode) then
       fileIOError path PermissionDenied "openFile" "Permission denied"
     else
       mkFile path >>= getFileContents "openFile"

readFile :: FilePath -> IO String
readFile = fmap T.unpack . readFileText

writeFileText :: FilePath -> Text -> IO ()
writeFileText path t =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p WriteMode) then
       fileIOError path PermissionDenied "openFile" "Permission denied"
     else do
       f <- mkFile path
       putFileContents f t

writeFile :: FilePath -> String -> IO()
writeFile path t = writeFileText path (T.pack t)

appendFileText :: FilePath -> Text -> IO ()
appendFileText path t =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p AppendMode) then
       fileIOError path PermissionDenied "openFile" "Permission denied"
     else do
       f <- mkFile path
       w <- getWorld
       case M.lookup f (files w) of
         Nothing -> putFileContents f t
         Just t' -> putFileContents f (T.append t' t)

appendFile :: FilePath -> String -> IO()
appendFile path t = appendFileText path (T.pack t)

readIO :: Read a => String -> IO a
readIO s        =  case (do { (x,t) <- reads s ;
                              ("","") <- lex t ;
                              return x }) of
                        [x]    -> return x
                        []     -> ioError (userError "Prelude.readIO: no parse")
                        _      -> ioError (userError "Prelude.readIO: ambiguous parse")

_hCanBlock :: Handle -> Bool
_hCanBlock h = h == stdin

wrapBlockingOp' :: String -> (Handle -> IO a) -> Handle -> IO (Maybe a)
wrapBlockingOp' s op h
  | h == stdin || h == stdout =
      do getHData s h >>= hEnsureReadable s h -- make sure this is not the user trying to getLine or something like that
         hEnsureOpen s h
         eof <- hIsEOF h
         if not eof then fmap Just (op h) else do
           if h == stdin then void runUser else wait
           eof <- hIsEOF h
           if eof then return Nothing else fmap Just (op h)
  | otherwise = fmap Just (op h)

wrapBlockingOp :: String -> (Handle -> IO a) -> Handle -> IO a
wrapBlockingOp s op h = wrapBlockingOp' s op h >>= maybe (hIOError h ReadDeadlock s msg) return
  where msg = if h == stdin then 
                "user input expected, but user does not respond"
              else if h == stdout then
                "IO thread expects user input, user expects IO output"
              else
                "the impossible happened"

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h _ = getHData "hWaitForInput" h >>= hEnsureReadable "hWaitForInput" h >> 
  fmap (maybe False (const True)) (wrapBlockingOp' "hWaitForInput" (const (return ())) h)

getText :: Integer -> IO Text
getText = hGetText stdin

getChar :: IO Char
getChar = hGetChar stdin

getLineText :: IO Text
getLineText = hGetLineText stdin

getLine :: IO String
getLine = hGetLine stdin

lookAhead :: IO Char
lookAhead = hLookAhead stdin

hGetText :: Handle -> Integer -> IO Text
hGetText h s = wrapBlockingOp "hGetText" (\h -> _hGetText h s) h

hLookAhead :: Handle -> IO Char
hLookAhead = wrapBlockingOp "hLookAhead" _hLookAhead

hGetLineText :: Handle -> IO Text
hGetLineText = wrapBlockingOp "hGetLine" _hGetLineText


readLn :: Read a => IO a
readLn = getLine >>= readIO

getContentText :: IO Text
getContentText = hGetContentText stdin

hGetContents :: Handle -> IO String
hGetContents h = fmap T.unpack (hGetContentText h)

getContents :: IO String
getContents = fmap T.unpack getContentText

interact :: (String -> String) -> IO ()
interact f = do
  s <- getContents
  putStr (f s)

hFlush :: Handle -> IO ()
hFlush _ = return ()


newtype ConsoleHook = ConsoleHook (MVar [(SpecialFile, Text)]) deriving (Typeable)

hookConsole :: IO ConsoleHook
hookConsole = 
    do v <- newMVar []
       registerWriteHook (hook v)
       return (ConsoleHook v)
  where hook v h t = case _hOutFile h of
                       SpecialFile special -> modifyMVar_ v (\xs -> return ((special, t) : xs))
                       _ -> return ()

readConsoleHook :: ConsoleHook -> IO [(SpecialFile, Text)]
readConsoleHook (ConsoleHook v) = fmap reverse (readMVar v)

showConsoleHook :: ConsoleHook -> IO String
showConsoleHook h = fmap (T.unpack . T.unlines . format) (readConsoleHook h)
  where format = map (\xs -> formatLine (fst (head xs)) (T.lines (T.concat (map snd xs)))) . groupBy ((==) `on` fst)
        formatLine t xs = T.intercalate nl (map (T.append (prefix t)) xs)
        nl = T.pack "\n"
        prefix StdIn  = "> "
        prefix StdOut = ""
        prefix StdErr = "ERR: "

newtype HandleHook = HandleHook (MVar [(Direction, Text)]) deriving (Typeable)

hookHandle :: Handle -> IO HandleHook
hookHandle h =
    do v <- newMVar []
       registerWriteHook (hook v)
       return (HandleHook v)
  where hook v h' t = if _hOutFile h' == _hOutFile h then
                        modifyMVar_ v (\xs -> return ((Out, t) : xs))
                      else if _hOutFile h' == _hInFile h then
                        modifyMVar_ v (\xs -> return ((In, t) : xs))
                      else
                        return ()

readHandleHook :: HandleHook -> IO [(Direction, Text)]
readHandleHook (HandleHook v) = fmap reverse (readMVar v)

showHandleHook :: HandleHook -> IO String
showHandleHook h = fmap (T.unpack . T.unlines . format) (readHandleHook h)
  where format = map (\xs -> formatLine (fst (head xs)) (T.lines (T.concat (map snd xs)))) . groupBy ((==) `on` fst)
        formatLine t xs = T.intercalate nl (map (T.append (prefix t)) xs)
        nl = T.pack "\n"
        prefix In  = "<< "
        prefix Out = ">> "


