module Types where

import Control.Concurrent.Lock (Lock())
import Control.Monad.Reader (ReaderT())

import Data.Text (Text())

import Web.Scotty.Trans (ActionT)

import Config

type DatabaseT = ReaderT (Lock, Configuration) IO

type Action a = ActionT Text DatabaseT a
