{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Control.Concurrent (forkIO)
import Control.Monad (when)

import Data.Maybe (fromJust, isNothing)

import qualified Database.Persist.Sqlite as Sq

import Web.Scotty (scotty)

import App
import Config (loadConfig, db, port)
import Model (migrateAll)
import Submit

main :: IO ()
main = do
  config' <- loadConfig "wxcs.conf"
  when (isNothing config') $ error "Config file (wxcs.conf) not found"
  let config = fromJust config'
  let db_file = db config

  Sq.runSqlite db_file $ Sq.runMigration migrateAll
  -- TODO: error handling?
  _ <- forkIO $ loop config
  scotty (port config) $ app db_file
