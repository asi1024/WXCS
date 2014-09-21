{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

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

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = do
  config' <- loadConfig "wxcs.conf"
  when (isNothing config') $ error "Config file (wxcs.conf) not found"
  let config = fromJust config'
  let dbFile = db config
  submitQueue <- atomically newTQueue

  Sq.runSqlite dbFile $ Sq.runMigration migrateAll
  Sq.withSqlitePool dbFile openConnectionCount $ \pool -> do
    let run = flip runReaderT (pool, config)
    run $ initializeSubmitQueue submitQueue
    forkIO_ $ run $ crawler submitQueue
    scottyT (port config) (`runReaderT` (pool, config))
      (`runReaderT` (pool, config)) (app submitQueue)
