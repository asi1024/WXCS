module Utils (
  getLocalTime,
  toZonedTime,
  showTime
  ) where

import Data.Time

import System.Locale (defaultTimeLocale)

toZonedTime :: String -> IO ZonedTime
toZonedTime s = do
  timezone <- getCurrentTimeZone
  return $ readTime defaultTimeLocale "%Y%m%d%H%M%S%z"
    (s ++ (timeZoneOffsetString timezone))

showTime :: ZonedTime -> String
showTime t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

getLocalTime :: IO String
getLocalTime = getZonedTime >>= (return . showTime)
