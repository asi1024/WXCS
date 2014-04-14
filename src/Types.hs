module Types where

import Control.Concurrent.Lock (Lock())
import Control.Monad.Reader (ReaderT())

import Config

type DatabaseT = ReaderT (Lock, Configuration) IO
