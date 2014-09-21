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
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT)

import Data.Time
import Database.Persist.Sql (SqlPersistM, runSqlPool)

import System.Locale (defaultTimeLocale)

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

runSql :: SqlPersistM a -> DatabaseT a
runSql action = do
  (pool, _) <- ask
  liftIO $ runResourceT $ runNoLoggingT $ runSqlPool action pool

forkIO_ :: IO () -> IO ()
forkIO_ a = void $ forkIO a
