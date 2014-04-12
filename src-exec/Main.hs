{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Lock (new)
import Control.Monad.Reader

import Data.Maybe (fromJust, isNothing)

import qualified Database.Persist.Sqlite as Sq

import Web.Scotty (scotty)

import App
import Config (loadConfig, db, port)
import Model (migrateAll)
import Submit
import Utils

main :: IO ()
main = do
  config' <- loadConfig "wxcs.conf"
  when (isNothing config') $ error "Config file (wxcs.conf) not found"
  let config = fromJust config'
  let db_file = db config

  Sq.runSqlite db_file $ Sq.runMigration migrateAll
  -- TODO: error handling?
  lock <- new
  forkIO_ $ runReaderT crawler (lock, config)
  scotty (port config) $ app config lock
