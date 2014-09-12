module Utils (
  forkIO_,
  getLocalTime,
  toZonedTime,
  fromZonedTime,
  diffTime,
  runSql,
  showTime,
  whenDef
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Lock (acquire, release)
import Control.Exception (bracket_)
import Control.Monad.Logger (NoLoggingT())
import Control.Monad.Reader
import Control.Monad.Trans.Resource (ResourceT)

import Data.Time
import qualified Database.Persist.Sqlite as Sq

import System.Locale (defaultTimeLocale)

import Config
import Types

toZonedTime :: String -> IO ZonedTime
toZonedTime s = do
  timezone <- getCurrentTimeZone
  return $ readTime defaultTimeLocale "%Y%m%d%H%M%S %Z"
    (s ++ " " ++ show timezone)

fromZonedTime :: ZonedTime -> String
fromZonedTime = formatTime defaultTimeLocale "%Y%m%d%H%M%S"

showTime :: ZonedTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

diffTime :: ZonedTime -> ZonedTime -> Int
diffTime a b = ceiling $ diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b) / 60

getLocalTime :: IO String
getLocalTime = liftM showTime getZonedTime

whenDef :: (Monad m) => a -> Bool -> m a -> m a
whenDef def p act = if p then act else return def

runSql :: Sq.SqlPersistT (NoLoggingT (ResourceT IO)) a -> DatabaseT a
runSql action = do
  (lock, conf) <- ask
  liftIO $ bracket_ (acquire lock) (release lock) (Sq.runSqlite (db conf) action)

forkIO_ :: IO () -> IO ()
forkIO_ a = void $ forkIO a
