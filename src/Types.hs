module Types where

import Control.Concurrent.Lock (Lock())
import Control.Monad.Reader (ReaderT())

import Config

type DatabaseT a = ReaderT (Lock, Configuration) IO a
