{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.IO (
    IO, Handle, IOMode (..), SeekMode (..), FilePath, HandlePosn,
    IOException (..), IOErrorType (..), BufferMode (..),
    stdin, stdout, stderr,
    withFile, openFile, hClose, readFile, writeFile, appendFile, doesFileExist,
    hFileSize, hSetFileSize, hIsEOF, isEOF, hReady, hAvailable, hWaitForInput,
    hGetPosn, hSetPosn, hSeek, hTell, hGetBuffering, hSetBuffering, hFlush,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable, hIsTerminalDevice, hShow,
    hGetChar, hGetLine, hLookAhead, hGetContents, 
    hPutChar, hPutStr, hPutStrLn, hPrint,
    putChar, putStr, putStrLn, print,
    ioError, ioException, userError,
    getChar, getLine, getContents, readIO, readLn, interact,
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation, 
    isPermissionError, isUserError, ioeGetErrorType, ioeGetLocation, ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    isReadDeadlockError, isAcceptDeadlockError
  ) where
  
import Mock.System.IO.Internal
