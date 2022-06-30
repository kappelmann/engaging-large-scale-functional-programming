module Competition.Logger (logStdout, logStderr) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import System.IO (hPutStrLn, stderr)

logStderr :: String -> IO ()
logStderr message = prependDateTime message >>= hPutStrLn stderr

logStdout :: String -> IO ()
logStdout message = prependDateTime message >>= putStrLn

prependDateTime :: String -> IO String
prependDateTime message = (++ (' ' : message)) <$> currentTimeString

currentTimeString :: IO String
currentTimeString = do
  timeZone <- getCurrentTimeZone
  utcTime <- getCurrentTime
  let format = iso8601DateFormat (Just "%H:%M:%S")
  let localTime = utcToLocalTime timeZone utcTime
  return $ formatTime defaultTimeLocale format localTime
