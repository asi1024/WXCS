module Utils (
  forkIO_,
  getLocalTime,
  toZonedTime,
  fromZonedTime,
  runSql,
  runSqlWithLock,
  showTime,
  whenDef
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Lock (Lock(), acquire, release)
import Control.Exception (bracket_)
import Control.Monad.Logger (NoLoggingT())
import Control.Monad.Reader

import Data.Conduit (ResourceT())
import Data.Text (Text())
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
showTime t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

getLocalTime :: IO String
getLocalTime = getZonedTime >>= (return . showTime)

whenDef :: (Monad m) => a -> Bool -> m a -> m a
whenDef def p act = if p then act else return def

runSqlWithLock :: Lock -> Text -> Sq.SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runSqlWithLock lock dbFile action =
  bracket_ (acquire lock) (release lock) (Sq.runSqlite dbFile action)

runSql :: Sq.SqlPersistT (NoLoggingT (ResourceT IO)) a -> DatabaseT a
runSql action = do
  (lock, conf) <- ask
  liftIO $ bracket_ (acquire lock) (release lock) (Sq.runSqlite (db conf) action)

forkIO_ :: IO () -> IO ()
forkIO_ a = forkIO a >> return ()
