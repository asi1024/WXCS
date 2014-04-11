module Utils (
  getLocalTime,
  toZonedTime,
  fromZonedTime,
  runSqlWithLock,
  showTime,
  whenDef
  ) where

import Control.Concurrent.Lock (Lock(), acquire, release)
import Control.Exception (bracket_)
import Control.Monad.Logger (NoLoggingT())

import Data.Conduit (ResourceT())
import Data.Text (Text())
import Data.Time
import qualified Database.Persist.Sqlite as Sq

import System.Locale (defaultTimeLocale)

toZonedTime :: String -> IO ZonedTime
toZonedTime s = do
  timezone <- getCurrentTimeZone
  return $ readTime defaultTimeLocale "%Y%m%d%H%M%S %Z"
    (s ++ " " ++ show timezone)

fromZonedTime :: ZonedTime -> String
fromZonedTime = formatTime defaultTimeLocale "%Y%m%d%H%M%S"

showTime :: ZonedTime -> String
showTime t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

getLocalTime :: IO String
getLocalTime = getZonedTime >>= (return . showTime)

whenDef :: (Monad m) => a -> Bool -> m a -> m a
whenDef def p act = if p then act else return def

runSqlWithLock :: Lock -> Text -> Sq.SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runSqlWithLock lock dbFile action =
  bracket_ (acquire lock) (release lock) (Sq.runSqlite dbFile action)
