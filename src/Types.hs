module Types where

import Control.Monad.Reader (ReaderT())

import Data.Text (Text())

import Database.Persist.Sql (ConnectionPool)
import qualified Database.Persist.Sqlite as Sq

import Web.Scotty.Trans (ActionT)

import Config

type DatabaseT = ReaderT (ConnectionPool, Configuration) IO

type Action a = ActionT Text DatabaseT a
type Entities a = Action [Sq.Entity a]
