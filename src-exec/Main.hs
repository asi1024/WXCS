{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Lock (new)
import Control.Concurrent.STM.TQueue (newTQueue)
import Control.Monad.Reader
import Control.Monad.STM (atomically)

import Data.Maybe (fromJust, isNothing)

import qualified Database.Persist.Sqlite as Sq

import Web.Scotty.Trans (scottyT)

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
  let dbFile = db config
  submitQueue <- atomically newTQueue

  Sq.runSqlite dbFile $ Sq.runMigration migrateAll
  -- TODO: error handling?
  lock <- new
  forkIO_ $ runReaderT (crawler submitQueue) (lock, config)
  scottyT (port config) (`runReaderT` (lock, config))
    (`runReaderT` (lock, config)) (app submitQueue)
